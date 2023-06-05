Please Note:
The boundary files for Papahanaumokuakea Marine National Monument (PMNM) are offered in both 
unprojected geographic coordinates and in an Albers Equal Area Conic projection. 

PMNM_py - unprojected geographic coordinates
PMNM_py_Albers - Albers Equal Area Conic projection

Both these GIS datasets are offered in separate projections due to the fact that the boundary for
PMNM crosses the International Date Line, and most software packeages are setup by default to
view the globe from -180° to 180° with 0° as the central meridian.

The Albers projection has been rotated so that -166.5° is the central meridian and will produce
boundary area that appears and acts as a single polygon. The unprojected geographic file will "act"
as a single polygon also, but will be split along the International Date Line into two sections appearing 
in the Eastern and Western Hemispheres if the central meridian is kept at 0°. If the central meridian
is changed/rotated, the polygon will appear to have a line running through it at the International 
Date Line. In either case it is only one polygon.

The Albers Equal Area Conic Projection used is a custom projection defined to best represent the 
Hawaiian Archipelago with minimal distortion.

10/21/2016