// Define the country boundary (Ghana)
var ghana = ee.FeatureCollection("FAO/GAUL/2015/level0")
                .filter(ee.Filter.eq("ADM0_NAME", "Ghana"));

ghana = ghana.geometry().buffer(1000);

// Load CHIRTS daily data
var chirts = ee.ImageCollection("UCSB-CHG/CHIRTS/DAILY")
                .filterDate("2000-01-01", "2010-12-31") // Adjust time period
                .select("maximum_temperature") // Maximum temperature in C
                .map(function(img) { return img.clip(ghana); }); // Clip to ghana

// Calculate 95th percentile temperature over the time period
var percentile95 = chirts.reduce(ee.Reducer.percentile([95]));

// Compute min and max values from the 95th percentile image
var minTemp = percentile95.reduceRegion({
  reducer: ee.Reducer.min(),
  geometry: ghana,
  scale: 5000,
  bestEffort: true
}).get("maximum_temperature_p95");

var maxTemp = percentile95.reduceRegion({
  reducer: ee.Reducer.max(),
  geometry: ghana,
  scale: 5000,
  bestEffort: true
}).get("maximum_temperature_p95");

print("Min Temp:", minTemp);
print("Max Temp:", maxTemp);

// Apply dynamic visualization
var visParams = {
  min: 30,
  max: 41,
  palette: ["blue", "cyan", "yellow", "orange", "red"]
};

// Display the updated layer
Map.addLayer(percentile95, visParams, "95th Percentile Temperature (°C)");

// Export the result as a GeoTIFF
Export.image.toDrive({
  image: percentile95,
  description: "Ghana_95th_Percentile_Temperature",
  scale: 5000,
  region: ghana,
  fileFormat: "GeoTIFF",
  crs: 'EPSG:4326',
  maxPixels: 1e13
});
