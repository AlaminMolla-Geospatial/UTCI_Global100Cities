// 
//           SEASON CONFIGURATION        


var SEASON_MODE   = 'SAME';   // 'DJF'  = Dec(Y-1) + Jan(Y) + Feb(Y)
                              // 'SAME' = all months within same year

var MONTH_START   = 6;        // used only when SEASON_MODE = 'SAME'
var MONTH_END     = 8;        // e.g. JFM=1-3, JJA=6-8, SON=9-11, MAM=3-5

var SEASON_LABEL  = 'JJA';   // label for output file & printed results

var DATA_START    = 2001;     // first year of your dataset
var DATA_END      = 2022;     // last year of your dataset

// ──────────────────────────────────────────────────────────────
// Derived — No EDIT BELOW THIS LINE
// ──────────────────────────────────────────────────────────────
var IS_DJF     = (SEASON_MODE === 'DJF');
var START_YEAR = IS_DJF ? DATA_START + 1 : DATA_START;

// All SAME-year seasons can start from 2001
var END_YEAR   = DATA_END;



//   CITY BOUNDARY                                           

var cityBoundary = ee.FeatureCollection("projects/ee-alaminswifl/assets/Osaka");
var cityGeom     = cityBoundary.geometry();


//   UTCI DATASET                                            

var utci = ee.ImageCollection("projects/sat-io/open-datasets/gloutci-m")
  .filterDate(DATA_START + '-01-01', DATA_END + '-12-31')
  .map(function(img) {
    var band = img.bandNames().get(0);
    return img.select([band])
      .divide(100)
      .rename('utci')
      .copyProperties(img, ['system:time_start']);
  });



//    MODIS LULC                                              

var modis = ee.ImageCollection('MODIS/061/MCD12Q1');



//  YEAR LIST                                               

var years = ee.List.sequence(START_YEAR, END_YEAR);



//   LULC RECLASSIFICATION                                   

function getZones(img) {
  // MODIS LC_Type1 class groupings
  var forest = img.remap([1,2,3,4,5,8,9], [1,1,1,1,1,1,1]).unmask(0).rename('forest');
  var shrub  = img.remap([6,7,10],         [1,1,1]        ).unmask(0).rename('shrub');
  var crop   = img.remap([12,14],          [1,1]          ).unmask(0).rename('crop');
  var barren = img.eq(16)                                  .unmask(0).rename('barren');
  var water  = img.remap([11,17],          [1,1]          ).unmask(0).rename('water');
  var urban  = img.eq(13)                                  .unmask(0).rename('urban');
  return { forest:forest, shrub:shrub, crop:crop,
           barren:barren, water:water, urban:urban };
}



//    PIXEL AREA (km²)                                        

var pixelArea = ee.Image.pixelArea().divide(1e6);



//    CLASS STATS (area km², mean UTCI, heat load)            

function classStats(mask, utciImg) {

  var area = ee.Number(
    pixelArea.updateMask(mask).reduceRegion({
      reducer:   ee.Reducer.sum(),
      geometry:  cityGeom,
      scale:     500,
      maxPixels: 1e13
    }).get('area')
  );
  area = ee.Number(ee.Algorithms.If(
    ee.Algorithms.IsEqual(area, null), ee.Number(0), area
  ));

  var meanUTCI = ee.Number(
    utciImg.updateMask(mask).reduceRegion({
      reducer:   ee.Reducer.mean(),
      geometry:  cityGeom,
      scale:     1000,
      maxPixels: 1e13
    }).get('utci')
  );
  meanUTCI = ee.Number(ee.Algorithms.If(
    ee.Algorithms.IsEqual(meanUTCI, null), ee.Number(0), meanUTCI
  ));

  return ee.Dictionary({
    area: area,
    utci: meanUTCI,
    heat: area.multiply(meanUTCI)
  });
}



//  SEASONAL UTCI HELPER                                    

function getSeasonalUTCI(y) {
  y = ee.Number(y);

  // ── DJF: Dec(Y-1) + Jan(Y) + Feb(Y) ──────────────────────────
  var djfCollection = utci
    .filter(ee.Filter.calendarRange(y.subtract(1), y.subtract(1), 'year'))
    .filter(ee.Filter.calendarRange(12, 12, 'month'))          // Dec of Y-1
    .merge(
      utci
        .filter(ee.Filter.calendarRange(y, y, 'year'))
        .filter(ee.Filter.calendarRange(1, 2, 'month'))        // Jan + Feb of Y
    );

  // ── SAME-year: any 3-month window within the same year ────────
  var sameYearCollection = utci
    .filter(ee.Filter.calendarRange(y, y, 'year'))
    .filter(ee.Filter.calendarRange(MONTH_START, MONTH_END, 'month'));

  // Select the correct collection based on mode
  var seasonal = IS_DJF ? djfCollection : sameYearCollection;

  return seasonal.mean().clip(cityGeom);
}



//  YEARLY STATS COMPUTATION                                

var yearly = ee.FeatureCollection(

  years.map(function(y) {
    y = ee.Number(y);

    // Seasonal mean UTCI
    var utciYear = getSeasonalUTCI(y);

    // MODIS LULC — use same year as Jan/Feb (year Y)
    var lulc = ee.Image(
      modis.filter(ee.Filter.calendarRange(y, y, 'year')).first()
    ).select('LC_Type1');

    var zones = getZones(lulc);

    // Per-class stats
    var forest = classStats(zones.forest, utciYear);
    var shrub  = classStats(zones.shrub,  utciYear);
    var crop   = classStats(zones.crop,   utciYear);
    var barren = classStats(zones.barren, utciYear);
    var water  = classStats(zones.water,  utciYear);
    var urban  = classStats(zones.urban,  utciYear);

    // Weighted mean UTCI across all classes
    var totalHeat = ee.Number(forest.get('heat'))
      .add(ee.Number(shrub.get('heat')))
      .add(ee.Number(crop.get('heat')))
      .add(ee.Number(barren.get('heat')))
      .add(ee.Number(water.get('heat')))
      .add(ee.Number(urban.get('heat')));

    var totalArea = ee.Number(forest.get('area'))
      .add(ee.Number(shrub.get('area')))
      .add(ee.Number(crop.get('area')))
      .add(ee.Number(barren.get('area')))
      .add(ee.Number(water.get('area')))
      .add(ee.Number(urban.get('area')));

    var weightedUTCI = ee.Number(
      ee.Algorithms.If(
        totalArea.gt(0),
        totalHeat.divide(totalArea),
        ee.Number(0)
      )
    );

    return ee.Feature(null, {
      year:          y,
      season:        SEASON_LABEL,

      forest_area:   forest.get('area'),
      shrub_area:    shrub.get('area'),
      crop_area:     crop.get('area'),
      barren_area:   barren.get('area'),
      urban_area:    urban.get('area'),

      forest_utci:   forest.get('utci'),
      shrub_utci:    shrub.get('utci'),
      crop_utci:     crop.get('utci'),
      barren_utci:   barren.get('utci'),
      urban_utci:    urban.get('utci'),

      weighted_utci: weightedUTCI
    });
  })
);



//  BASELINE (first available year of chosen season)       

var base = yearly.filter(ee.Filter.eq('year', START_YEAR)).first();



// RATE OF CHANGE (ROC) HELPER                          

function roc(current, baseline) {
  var c = ee.Number(current);
  var b = ee.Number(baseline);
  return ee.Number(
    ee.Algorithms.If(
      b.gt(0),
      c.subtract(b).divide(b).multiply(100),
      ee.Number(0)
    )
  );
}


//  ADD ROC COLUMNS                                        

var final = yearly.map(function(f) {
  return f.set({
    forest_roc:  roc(f.get('forest_area'), base.get('forest_area')),
    shrub_roc:   roc(f.get('shrub_area'),  base.get('shrub_area')),
    crop_roc:    roc(f.get('crop_area'),   base.get('crop_area')),
    barren_roc:  roc(f.get('barren_area'), base.get('barren_area')),
    water_roc:   roc(f.get('water_area'),  base.get('water_area')),
    urban_roc:   roc(f.get('urban_area'),  base.get('urban_area'))
  });
});



// PREVIEW                                                

print('Season : ' + SEASON_LABEL, final);
print('Years  : ' + START_YEAR + ' – ' + END_YEAR);
print('Mode   : ' + SEASON_MODE);



// EXPORT                                                 

Export.table.toDrive({
  collection:  final,
  description: 'Osaka_WeightedUTCI_' + SEASON_LABEL + '_' + START_YEAR + '_' + END_YEAR,
  folder:      'GlobalCITIES',
  fileFormat:  'CSV'
});