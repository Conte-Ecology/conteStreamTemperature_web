CREATE TEMPORARY VIEW dataset_view AS 
  SELECT a.name AS agency, l.name AS location, concat_ws('_', a.name, l.name) AS site,
         l.latitude AS latitude, l.longitude AS longitude, l.catchment_id AS catchment,
         v.datetime AT TIME ZONE 'UTC' AS DATE, v.value AS temp
  FROM values v
  LEFT JOIN series s ON v.series_id=s.id
  LEFT JOIN agencies a ON s.agency_id=a.id
  LEFT JOIN locations l ON s.location_id=l.id
  LEFT JOIN variables var ON s.variable_id=var.id
  WHERE var.name='TEMP'
  ORDER BY agency, site, date;

\COPY (SELECT * FROM dataset_view) TO 'temperatureData.csv' CSV HEADER;
