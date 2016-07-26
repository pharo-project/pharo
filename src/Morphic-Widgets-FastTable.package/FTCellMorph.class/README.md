I represent a cell for the table. 

Description
------------------

I contains other Morph that need to be render. I should be answered in FTDataSource>>#cellTable:column:row:, with this method a FTTableContainer can display all the needed cells.

Technically, FTDataSource can answer  any kind of morph, but I'm better prepared  for the role so is recommendable  to  use me.

Public API and Key Messages
-------------------

- #withTopSeparator 		this is use to put a separator before the cell. (For exmple on a FTOutlineDataSource).

Example
-------------------

FTCellMorph new
		addMorphBack: 'Example' asMorph;
		withTopSeparator;
		openInWindow.
 
Internal Representation and Key Implementation Points.
-----------------

    Instance Variables
	topSeparator:		This is a boolean to know if the cell need a topSeparator.
