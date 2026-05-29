// ============================================================
//  LCZ × UTCI  —  City-level heat exposure by urban form
//  Dataset 1      : RUB/RUBCLIM/LCZ/global_lcz_map/latest  (100 m, static)
//  Dataset 2      : sat-io/open-datasets/gloutci-m          (1 km)
//
//  This is a SINGLE-SNAPSHOT analysis:
//    • LCZ map is static (one map, no year loop)
//    • UTCI is extracted for ONE season only
//    • SAME-year seasons (MAM, JJA, SON, etc.) use UTCI from 2018
//    • DJF uses Dec 2017 + Jan 2018 + Feb 2018
//    • Output: one row per city in the CSV
//    • ALL missing / absent values → -999  (no blank cells)
// ============================================================


// ──────────────────────────────────────────────────────────────
//  SEASON CONFIGURATION  — edit only this block
// ──────────────────────────────────────────────────────────────

var SEASON_MODE  = 'SAME';  // 'DJF'  = Dec 2017 + Jan 2018 + Feb 2018
                             // 'SAME' = selected months within 2018

var MONTH_START  = 3;        // used only when SEASON_MODE = 'SAME'
var MONTH_END    = 5;        // e.g. JFM=1-3, JJA=6-8, SON=9-11, MAM=3-5

var SEASON_LABEL = 'MAM';   // label written to the output CSV

// UTCI anchor year:
//   SAME-year seasons → months MONTH_START–MONTH_END of UTCI_YEAR
//   DJF              → Dec (UTCI_YEAR-1) + Jan–Feb UTCI_YEAR
var UTCI_YEAR    = 2018;

// ──────────────────────────────────────────────────────────────
//  Derived — no edits below this line
// ──────────────────────────────────────────────────────────────
var IS_DJF  = (SEASON_MODE === 'DJF');
var NODATA  = -999;   // sentinel for ALL missing / absent values


// ============================================================
//  1.  CITY BOUNDARY
// ============================================================

var cityBoundary = ee.FeatureCollection(
  "projects/ee-alaminswifl/assets/Yangon"
);
var cityGeom  = cityBoundary.geometry();
var CITY_NAME = 'Yangon';


// ============================================================
//  2.  UTCI COLLECTION  (monthly mean, °C equivalent)
//      Load 2017–2018 to cover DJF (Dec 2017) and all 2018 seasons
// ============================================================

var utci = ee.ImageCollection("projects/sat-io/open-datasets/gloutci-m")
  .filterDate('2017-01-01', '2018-12-31')
  .map(function(img) {
    var band = img.bandNames().get(0);
    return img.select([band])
      .divide(100)        // stored ×100 → °C
      .rename('utci')
      .copyProperties(img, ['system:time_start']);
  });


// ============================================================
//  3.  SEASONAL UTCI IMAGE  (single mean composite)
// ============================================================

var seasonalUTCI = (function() {
  if (IS_DJF) {
    // Dec of (UTCI_YEAR - 1)  +  Jan & Feb of UTCI_YEAR
    return utci
      .filter(ee.Filter.calendarRange(UTCI_YEAR - 1, UTCI_YEAR - 1, 'year'))
      .filter(ee.Filter.calendarRange(12, 12, 'month'))
      .merge(
        utci
          .filter(ee.Filter.calendarRange(UTCI_YEAR, UTCI_YEAR, 'year'))
          .filter(ee.Filter.calendarRange(1, 2, 'month'))
      )
      .mean().clip(cityGeom);
  } else {
    return utci
      .filter(ee.Filter.calendarRange(UTCI_YEAR, UTCI_YEAR, 'year'))
      .filter(ee.Filter.calendarRange(MONTH_START, MONTH_END, 'month'))
      .mean().clip(cityGeom);
  }
})();


// ============================================================
//  4.  LCZ  —  static map, 100 m, band: LCZ_Filter
// ============================================================

// IMPORTANT: The LCZ dataset is a tiled ImageCollection.
// .first() returns only ONE tile (typically Europe) and will
// produce empty results for cities outside that tile (e.g. Africa).
// .mosaic() merges all tiles into a single global image — required
// for any city outside the default first tile.
var lczImage = ee.ImageCollection(
  "RUB/RUBCLIM/LCZ/global_lcz_map/latest"
).mosaic().select('LCZ_Filter');

/*
  LCZ super-class groupings
  ─────────────────────────
  urban_compact              :  1, 2, 3, 7, 10
  urban_open                 :  4, 5, 6, 8, 9
  trees                : 11, 12
  bush_shrub_lowplants : 13, 14
  other_impervious     : 15, 16
  water                : 17

  Pixels outside these values (e.g. 0 = unclassified) are
  excluded from all area and UTCI calculations.
*/

var compactMask  = lczImage.remap([1,2,3,7,10], [1,1,1,1,1], 0).rename('urban_compact');
var openMask     = lczImage.remap([4,5,6,8,9],  [1,1,1,1,1], 0).rename('urban_open');
var treesMask    = lczImage.remap([11,12],       [1,1],       0).rename('trees');
var bushMask     = lczImage.remap([13,14],       [1,1],       0).rename('bush_shrub_lowplants');
var otherImpMask = lczImage.remap([15,16],       [1,1],       0).rename('other_impervious');
var waterMask    = lczImage.remap([17],          [1],         0).rename('water');

var LCZ_CLASSES = [
  { name: 'urban_compact',              mask: compactMask   },
  { name: 'urban_open',                 mask: openMask      },
  { name: 'trees',                mask: treesMask     },
  { name: 'bush_shrub_lowplants', mask: bushMask      },
  { name: 'other_impervious',     mask: otherImpMask  },
  { name: 'water',                mask: waterMask     }
];


// ============================================================
//  5.  PIXEL AREA helper (km²)
// ============================================================

var pixelArea_km2 = ee.Image.pixelArea().divide(1e6);


// ============================================================
//  6.  RESAMPLE LCZ MASKS: 100 m → 1 km  (fractional coverage)
//
//  Bilinear resampling of each binary mask yields a value 0–1
//  representing the fraction of the 1-km cell covered by that
//  class. Used as spatial weight in the UTCI mean calculation.
//  Area is always measured at native 100 m (no resampling).
// ============================================================

function resampleMaskTo1km(binaryMask) {
  return binaryMask
    .reproject({ crs: 'EPSG:4326', scale: 100 })
    .resample('bilinear')
    .reproject({ crs: 'EPSG:4326', scale: 1000 });
}

var resampledMasks = LCZ_CLASSES.map(function(cls) {
  return { name: cls.name, mask: resampleMaskTo1km(cls.mask) };
});


// ============================================================
//  7.  TOTAL CITY AREA  (km², native 100 m, valid LCZ only)
// ============================================================

var validLCZMask = compactMask
  .or(openMask).or(treesMask).or(bushMask)
  .or(otherImpMask).or(waterMask);

var _totalAreaRaw = pixelArea_km2
  .updateMask(validLCZMask.eq(1))
  .reduceRegion({
    reducer:   ee.Reducer.sum(),
    geometry:  cityGeom,
    scale:     100,
    maxPixels: 1e13
  }).get('area');

var totalCityArea_km2 = ee.Number(
  ee.Algorithms.If(
    ee.Algorithms.IsEqual(_totalAreaRaw, null),
    ee.Number(NODATA),
    _totalAreaRaw
  )
);


// ============================================================
//  8.  HELPER — safe number extraction from a Dictionary
//      Returns NODATA (-999) if the key is null/absent,
//      guaranteeing no blank cells in the output CSV.
// ============================================================

function safeGet(dict, key) {
  var val = dict.get(key);
  return ee.Number(
    ee.Algorithms.If(ee.Algorithms.IsEqual(val, null), ee.Number(NODATA), val)
  );
}


// ============================================================
//  9.  PER-CLASS STATISTICS
// ============================================================

// ── 9a. Area (km²) at native 100-m resolution ───────────────
function classArea_km2(binaryMask100m) {
  var dict = pixelArea_km2
    .updateMask(binaryMask100m.eq(1))
    .reduceRegion({
      reducer:   ee.Reducer.sum(),
      geometry:  cityGeom,
      scale:     100,
      maxPixels: 1e13
    });
  return safeGet(dict, 'area');
}

// ── 9b. Area-weighted mean UTCI at 1-km resolution ──────────
//    Weighted mean = Σ(utci × fraction) / Σ(fraction)
//    where fraction = resampled LCZ mask value [0–1]
function classMeanUTCI(resampledFracMask, utciImg) {

  var sumDict = utciImg.multiply(resampledFracMask)
    .reduceRegion({
      reducer:   ee.Reducer.sum(),
      geometry:  cityGeom,
      scale:     1000,
      maxPixels: 1e13
    });
  var weightDict = resampledFracMask
    .reduceRegion({
      reducer:   ee.Reducer.sum(),
      geometry:  cityGeom,
      scale:     1000,
      maxPixels: 1e13
    });

  // safeGet for numerator; for denominator grab first band value
  var weightedSum = safeGet(sumDict, 'utci');

  var _wRaw = weightDict.values().get(0);
  var totalWeight = ee.Number(
    ee.Algorithms.If(ee.Algorithms.IsEqual(_wRaw, null), ee.Number(0), _wRaw)
  );

  // If numerator itself is -999 (no pixels) → return -999
  // If denominator is 0 → return -999
  return ee.Number(
    ee.Algorithms.If(
      weightedSum.eq(NODATA).or(totalWeight.lte(0)),
      ee.Number(NODATA),
      weightedSum.divide(totalWeight)
    )
  );
}

// ── 9c. Proportion (%) of total valid LCZ area ──────────────
//    Propagates -999 if area is -999 or total city area is -999/0
function classProp(area_km2) {
  return ee.Number(
    ee.Algorithms.If(
      // Either value is -999 or total is zero → no valid proportion
      area_km2.eq(NODATA)
        .or(totalCityArea_km2.eq(NODATA))
        .or(totalCityArea_km2.lte(0)),
      ee.Number(NODATA),
      area_km2.divide(totalCityArea_km2).multiply(100)
    )
  );
}


// ============================================================
//  10.  ASSEMBLE OUTPUT FEATURE  (single row)
// ============================================================

var featureProps = {
  city:        CITY_NAME,
  season:      SEASON_LABEL,
  utci_year:   UTCI_YEAR,
  utci_period: IS_DJF
                 ? ('Dec' + (UTCI_YEAR - 1) + '_Jan-Feb' + UTCI_YEAR)
                 : (UTCI_YEAR + '_m' + MONTH_START + 'to' + MONTH_END),
  total_area_km2: totalCityArea_km2
};

var totalHeat             = ee.Number(0);
var totalAreaForWeighting = ee.Number(0);

for (var i = 0; i < LCZ_CLASSES.length; i++) {
  var cls   = LCZ_CLASSES[i];
  var rMask = resampledMasks[i].mask;

  var area  = classArea_km2(cls.mask);
  var prop  = classProp(area);
  var meanU = classMeanUTCI(rMask, seasonalUTCI);

  // Always write all columns to CSV — including water
  featureProps[cls.name + '_area_km2']  = area;
  featureProps[cls.name + '_prop_pct']  = prop;
  featureProps[cls.name + '_utci_mean'] = meanU;

  // City-wide weighted UTCI: EXCLUDE water (LCZ 17).
  // Water UTCI is retained in the CSV for reporting but must not
  // bias the urban/vegetated thermal comfort index — water bodies
  // have fundamentally different thermal dynamics to built/green surfaces.
  // isWater is a plain JS boolean resolved at script-parse time,
  // so ee.Algorithms.If receives a concrete true/false, not an ee.Boolean.
  var isWater   = (cls.name === 'water');
  var bothValid = area.gt(0).and(area.neq(NODATA))
                            .and(meanU.neq(NODATA));

  totalHeat = totalHeat.add(
    ee.Number(ee.Algorithms.If(
      isWater ? false : bothValid,
      area.multiply(meanU),
      ee.Number(0)
    ))
  );
  totalAreaForWeighting = totalAreaForWeighting.add(
    ee.Number(ee.Algorithms.If(
      isWater ? false : bothValid,
      area,
      ee.Number(0)
    ))
  );
}

// City-wide area-weighted mean UTCI — non-water LCZ classes only.
// Water proportion and UTCI are exported in the CSV but excluded here.
// Returns -999 if no valid non-water classes exist in the city.
featureProps['city_utci_weighted_nowater'] = ee.Number(
  ee.Algorithms.If(
    totalAreaForWeighting.gt(0),
    totalHeat.divide(totalAreaForWeighting),
    ee.Number(NODATA)
  )
);
var result = ee.FeatureCollection([ee.Feature(null, featureProps)]);


// ============================================================
//  11.  CONSOLE PREVIEW
// ============================================================

print('=== LCZ × UTCI  |  ' + CITY_NAME + ' ===');
print('Season  : ' + SEASON_LABEL + '  (' + SEASON_MODE + ')');
print('UTCI yr : ' + UTCI_YEAR);
if (IS_DJF) {
  print('Months  : Dec ' + (UTCI_YEAR - 1) + ' + Jan–Feb ' + UTCI_YEAR);
} else {
  print('Months  : ' + MONTH_START + '–' + MONTH_END + ' of ' + UTCI_YEAR);
}
print('LCZ band: LCZ_Filter  (static, no year loop)');
print('Missing : -999  (no blank cells)');
print('');
print('Result:', result);


// ============================================================
//  12.  EXPORT TO GOOGLE DRIVE
//
//  Output CSV — ONE row per city, columns:
//  ─────────────────────────────────────────────────────────
//  city | season | utci_year | utci_period | total_area_km2
//  city_utci_weighted_nowater  (water excluded from weighting)
//  [for each of 6 LCZ classes]:
//    <class>_area_km2   -- absolute area (km2)
//    <class>_prop_pct   -- % of total valid LCZ area (sums ~100)
//    <class>_utci_mean  -- area-weighted mean UTCI (degC); water_utci_mean
//                          is reported but excluded from city_utci_weighted_nowater
//  All missing / absent values are written as -999
// ============================================================

Export.table.toDrive({
  collection:     result,
  description:    CITY_NAME + '_LCZ_UTCI_' + SEASON_LABEL + '_' + UTCI_YEAR,
  folder:         'GlobalCITIES',
  fileNamePrefix: CITY_NAME + '_LCZ_UTCI_' + SEASON_LABEL + '_' + UTCI_YEAR,
  fileFormat:     'CSV'
});