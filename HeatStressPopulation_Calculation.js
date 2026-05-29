// ── CONFIG ────────────────────────────────────────────────────
var SEASON_MODE    = 'SAME';  // 'DJF' or 'SAME'
var MONTH_START    = 3;
var MONTH_END      = 5;
var SEASON_LABEL   = 'MAM';
var DATA_START     = 2000;
var DATA_END       = 2022;
var T_MODERATE     = 26;
var T_STRONG       = 32;
var T_VERY_STRONG  = 38;
var T_EXTREME      = 46;

var IS_DJF = (SEASON_MODE === 'DJF');

// If DJF: use 2001 & 2020
var POP_YEARS = IS_DJF ? [2001, 2020] : [2000, 2020];

// ── CITY ──────────────────────────────────────────────────────
var cityGeom = ee.FeatureCollection("projects/ee-alaminswifl/assets/Yangon").geometry();

// ── DATASETS ──────────────────────────────────────────────────
var utciCol = ee.ImageCollection("projects/sat-io/open-datasets/gloutci-m")
  .filterDate(DATA_START + '-01-01', DATA_END + '-12-31')
  .map(function(img) {
    var band = img.bandNames().get(0);
    return img.select([band]).divide(100).rename('utci')
      .copyProperties(img, ['system:time_start']);
  });

var worldpopCol = ee.ImageCollection('WorldPop/GP/100m/pop');

// ── FUNCTION: TOTAL POPULATION FOR A YEAR ─────────────────────
function getTotalPop(year) {
  var popImg = worldpopCol
    .filter(ee.Filter.eq('year', year))
    .mosaic()
    .rename('population')
    .clip(cityGeom);

  var total = popImg.reduceRegion({
    reducer: ee.Reducer.sum(),
    geometry: cityGeom,
    scale: 100,
    maxPixels: 1e13
  }).get('population');

  return ee.Number(ee.Algorithms.If(ee.Algorithms.IsEqual(total, null), 0, total));
}

// Compute once (IMPORTANT)
var totalpop_2000 = getTotalPop(2000);
var totalpop_2020 = getTotalPop(2020);

// ── SEASONAL UTCI ─────────────────────────────────────────────
function getSeasonalUTCI(y) {
  y = ee.Number(y);

  var djfCol = utciCol
    .filter(ee.Filter.calendarRange(y.subtract(1), y.subtract(1), 'year'))
    .filter(ee.Filter.calendarRange(12, 12, 'month'))
    .merge(
      utciCol
        .filter(ee.Filter.calendarRange(y, y, 'year'))
        .filter(ee.Filter.calendarRange(1, 2, 'month'))
    );

  var sameYearCol = utciCol
    .filter(ee.Filter.calendarRange(y, y, 'year'))
    .filter(ee.Filter.calendarRange(MONTH_START, MONTH_END, 'month'));

  return (IS_DJF ? djfCol : sameYearCol)
    .mean()
    .clip(cityGeom);
}

// ── MAIN LOOP ─────────────────────────────────────────────────
var results = ee.FeatureCollection(ee.List(POP_YEARS).map(function(yr) {
  yr = ee.Number(yr);

  // UTCI 3-year averaging
  var utciYears = ee.Algorithms.If(
    yr.eq(2001),
    [2001, 2002, 2003],
    [2020, 2021, 2022]
  );

  var utciImg = ee.ImageCollection.fromImages(
    ee.List(utciYears).map(function(y) {
      return getSeasonalUTCI(y);
    })
  ).mean();

  // WorldPop year logic
  var popYear = ee.Number(ee.Algorithms.If(yr.eq(2001), 2000, yr));

  // Aggregate 100 m → 1 km
  var pop1km = worldpopCol
    .filter(ee.Filter.eq('year', popYear))
    .mosaic()
    .rename('population')
    .clip(cityGeom)
    .setDefaultProjection({ crs: 'EPSG:4326', scale: 100 })
    .reduceResolution({ reducer: ee.Reducer.sum().unweighted(), maxPixels: 1024 })
    .reproject({ crs: utciImg.projection(), scale: 1000 });

  // Stress masks
  var noStress   = utciImg.lt(T_MODERATE);
  var moderate   = utciImg.gte(T_MODERATE).and(utciImg.lt(T_STRONG));
  var strong     = utciImg.gte(T_STRONG).and(utciImg.lt(T_VERY_STRONG));
  var veryStrong = utciImg.gte(T_VERY_STRONG).and(utciImg.lt(T_EXTREME));
  var extreme    = utciImg.gte(T_EXTREME);

  function sumPop(mask) {
    var v = pop1km.updateMask(mask).reduceRegion({
      reducer: ee.Reducer.sum(),
      geometry: cityGeom,
      scale: 1000,
      maxPixels: 1e13
    }).get('population');

    return ee.Number(ee.Algorithms.If(ee.Algorithms.IsEqual(v, null), 0, v));
  }

  return ee.Feature(null, {
    year:            yr,
    worldpop_year:   popYear,
    season:          SEASON_LABEL,

    // NEW COLUMNS
    totalpop_2000: totalpop_2000,
    totalpop_2020: totalpop_2020,

    pop_no_stress:   sumPop(noStress),
    pop_moderate:    sumPop(moderate),
    pop_strong:      sumPop(strong),
    pop_very_strong: sumPop(veryStrong),
    pop_extreme:     sumPop(extreme)
  });
}));

// ── PREVIEW & EXPORT ──────────────────────────────────────────
print(results);

Export.table.toDrive({
  collection:  results,
  description: 'Yangon_PopStress_' + SEASON_LABEL + '_2000_2020',
  folder:      'GlobalCITIES',
  fileFormat:  'CSV'
});