CREATE TEMPORARY VIEW locations_view AS 
  SELECT a.name AS agency, l.name AS location, concat_ws('_', a.name, l.name) AS site,
         l.latitude AS latitude, l.longitude AS longitude, l.catchment_id AS catchment
  FROM locations l
  LEFT JOIN agencies a ON l.agency_id=a.id
  ORDER BY agency, site;

\COPY (SELECT * FROM locations_view) TO 'locationsData.csv' CSV HEADER;