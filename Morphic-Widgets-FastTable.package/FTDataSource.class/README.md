I am an abtract class to define the basic implementation of a DataSource. A DataSource is a Model for a FTTableMorph. My purpose is to provide content to it .

Description
--------------------

My subclasses will store some values that will be use to feed a FTTableMorph.
 I work with a FTTableMorph so I store one.

Public API and Key Messages
--------------------

- #elementAt: anIndex 		return an object to display for an index in the table.
		
- #numberOfRows 			return the max number of rows inside the table.
			
- #cellColumn: index1 row: idex2 		return a Morph (probably a FTCellMorph) that will be display in the table. The Morph need to contains the object at index2 and everything the user want to display for a data.
		
- #newDataSourceMatching: aRegex / #searchText: aString 		these methods are use with a FTFunction. For more information see FTSeachFunction or FTFilterFunction for mone information.

Example
-------------------

This is an Abstract class, see subclasses for examples.
 
Internal Representation and Key Implementation Points.
-------------------

    Instance Variables
	table:		A FTTableMorph that use the dataSource.
