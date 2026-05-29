// ======================================================
// 1. LOAD GDP DATA
// ======================================================
var gdp = ee.Image("projects/sat-io/open-datasets/GRIDDED_HDI_GDP/adm0_gdp_perCapita_1990_2022");

// ======================================================
// 2. CITY LIST
// ======================================================
var cityNames = [
  "Abidjan","AddisAbaba","Ahmedabad","Alexandria","Ankara","Bagdad","Bangalore",
  "Bangkok","Barcelona","Beijing","BeloHorizonte","Bogota","Brasilia","BuenosAires",
  "Cairo","CapeTown","Changsha","Chengdu","Chennai","Chittagong","Chongqing","Dalian",
  "DaresSalaam","Dhaka","Dongguan","Foshan","Fukuoka","Guadalajara","Guangzhou","Hangzhou",
  "Hanoi","Harbin","Hefei","HoChiMinhCity","HongKong","Hyderabad","Istanbul","Jakarta",
  "Jeddah","Jinan","Johannesburg","KabulCity","Karachi","Khartoum","Kinshasa","Kolkata",
  "KualaLumpur","Kunming","Lagos","Lahore","Lima","London","Luanda","Madrid","Manila",
  "Maricopa","Melbourne","MexicoCity","Monterrey","Montreal","Moscow","Mumbai","Nagoya",
  "Nairobi","Nanjing","NewDelhi","NewTaipeiCity","NewYork","Ningbo","Osaka","Paris","Pune",
  "Qingdao","Recife","RiodeJaneiro","Riyadh","Rome","SaintPetersburg","Santiago","SaoPaulo",
  "Seoul","Shanghai","Shantou","Shenyang","Shenzhen","Shijiazhuang","Singapore","Surat",
  "Sydney","Tehran","TelAviv","Tianjin","Tokyo","Toronto","Urumqi","Wuhan","XiAn","Yangon",
  "Yaounde","Zhengzhou"
];

// ======================================================
// 3. BUILD CITY FEATURE COLLECTION
// ======================================================
var cities = ee.FeatureCollection(
  cityNames.map(function(name) {
    var geom = ee.FeatureCollection('projects/ee-alaminswifl/assets/' + name).geometry();
    return ee.Feature(geom, { city: name });
  })
);

// ======================================================
// 4. SELECT YEARLY BANDS 2002–2022
// ======================================================
var years = [2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,
             2011,2012,2013,2014,2015,2016,2017,2018,2019,
             2020,2021,2022];

// ======================================================
// 5. EXTRACT MEAN GDP PER CITY PER YEAR
// ======================================================
var cityGDP = cities.map(function(feature) {
  
  var props = {};
  
  years.forEach(function(y) {
    var bandName = 'PPP_' + y;
    var val = gdp.select([bandName]).reduceRegion({
      reducer: ee.Reducer.mean(),
      geometry: feature.geometry(),
      scale: 1000,
      maxPixels: 1e13
    }).get(bandName);
    
    feature = feature.set('GDP_' + y, val);
  });
  
  return feature;
});

// ======================================================
// 6. PREVIEW
// ======================================================
print('First city GDP time series:', cityGDP.first());

// ======================================================
// 7. EXPORT — one row per city, one column per year
// ======================================================
Export.table.toDrive({
  collection: cityGDP,
  description: 'City_GDP_Yearly_2002_2022',
  folder: 'GlobalCITIES',
  fileNamePrefix: 'City_GDP_Yearly_2002_2022',
  fileFormat: 'CSV',
  selectors: ['city',
    'GDP_2001','GDP_2002','GDP_2003','GDP_2004','GDP_2005','GDP_2006',
    'GDP_2007','GDP_2008','GDP_2009','GDP_2010','GDP_2011',
    'GDP_2012','GDP_2013','GDP_2014','GDP_2015','GDP_2016',
    'GDP_2017','GDP_2018','GDP_2019','GDP_2020','GDP_2021',
    'GDP_2022'
  ]
});