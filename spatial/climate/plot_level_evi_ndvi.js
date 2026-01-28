// Purpose: Calculate vegetation indices from Sentinel-2 bands at each time step, aggregate for plot boundaries, select maximum by growing season, extract to CSV
// Author: Evelyn Shu

// Import plots
// var plots = ee.FeatureCollection("location_of_asset");

// Buffer by X meters (optional, depending on context)
var bufferedplots = plots.geometry().buffer(20);

// Load Sentinel-2 surface reflectance data
var sentinel2 = ee.ImageCollection('COPERNICUS/S2_HARMONIZED')
  .filterDate('2020-06-01', '2020-12-31') // Specify date range
  .filterBounds(bufferedplots);

// Cloud masking function using QA60
var maskClouds = function(image) {
  var cloudMask = image.select('QA60').eq(0); // Mask cloudy pixels
  return image.updateMask(cloudMask);
};

// Apply cloud masking to the image collection
var sentinel2Masked = sentinel2.map(maskClouds);

// Calculate EVI for each image in the collection
var calculateEVI = function(image) {
  var evi = image.expression(
    '2.5 * ((NIR - RED) / (NIR + 6 * RED - 7.5 * BLUE + 1))', {
      'NIR': image.select('B8').divide(10000),  // NIR band
      'RED': image.select('B4').divide(10000),  // Red band
      'BLUE': image.select('B2').divide(10000)  // Blue band
    }
  ).rename('EVI');
  return evi.copyProperties(image, image.propertyNames()); // Keep metadata
};

// OR - Calculate NDVI for each image in the collection. Update other names throughout, but same process
// var calculateNDVI = function(image) {
//   var ndvi = image.expression(
//     '(NIR - RED) / (NIR + RED)', {
//       'NIR': image.select('B8').divide(10000),  // Scale NIR
//       'RED': image.select('B4').divide(10000)   // Scale Red
//     }
//   ).rename('NDVI');
//   return ndvi.copyProperties(image, image.propertyNames()); // Keep metadata
// };

// Mask out extreme EVI values (e.g., > 1)
var maskExtremeEVI = function(image) {
  return image.updateMask(image.lte(1).and(image.gte(-1))); // Keep values between -1 and 1
};

// Apply the mask to the EVI collection
var sentinel2EVI = sentinel2Masked.map(calculateEVI).map(maskExtremeEVI);

// Calculate zonal statistics (here, using mean) for each plot at each time step
var zonalMeans = sentinel2EVI.map(function(image) {
  var means = image.reduceRegions({
    collection: plots,
    reducer: ee.Reducer.mean(),
    scale: 10, // Match Sentinel-2 resolution, can adjust
    crs: 'EPSG:4326'
  });
  // Add the date property and unique ID
  return means.map(function(feature) {
    return feature.set('date', image.date().format('yyyy-MM-dd'))
                  .set('uid', feature.get('uid')); 
  });
}).flatten();

// Find the maximum mean EVI and corresponding date for each plot (using 'uid')
var maxZonalMeans = zonalMeans
  .filter(ee.Filter.notNull(['mean'])) // Remove null values
  .reduceColumns({
    selectors: ['uid', 'mean', 'date'],
    reducer: ee.Reducer.max(2).setOutputs(['max_mean', 'max_date']).group({
      groupField: 0, // Group by 'uid'
      groupName: 'uid'
    }),
  });

// Convert grouped results to a FeatureCollection for export
var groupedResults = ee.FeatureCollection(ee.List(maxZonalMeans.get('groups')).map(function(item) {
  var dict = ee.Dictionary(item);
  return ee.Feature(null, {
    uid: dict.get('uid'),
    max_mean: dict.get('max_mean'),
    max_date: dict.get('max_date')
  });
}));

// Print the resulting FeatureCollection to the console
print('Maximum zonal mean EVI with dates:', groupedResults);

// Export plot values to Google Drive as a CSV
Export.table.toDrive({
  collection: groupedResults,
  description: 'file description here',
  fileFormat: 'CSV'
}); 
