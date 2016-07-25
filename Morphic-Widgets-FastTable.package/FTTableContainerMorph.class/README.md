I am a Morph that contain visible rows in a FTTableMorph. 

Description 
------------------

I am the main Morph of the FastTable that is responsible of displaying all the rows of a Table. 
My owner need to be a FTTableMorph and I will use his dataSource to display the needed informations.

Public API and Key Messages
-----------------
		
- #updateAllRows 

- #updateExposedRows

- #ipdateHeaderRow
 
Internal Representation and Key Implementation Points.
----------------

    Instance Variables
	exposedRows:		A dictionary of index/row with all the exposed rows.
	headerRow:			When not nil contains the header row of the container.
	needsRefreshExposedRows:		A boolean that is true if the container need a refresh. 

The method #drawOn: is responsible of my rendering.