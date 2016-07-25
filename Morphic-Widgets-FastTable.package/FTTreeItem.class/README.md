I am an abstract class to define an Item use by a tree data source of Fast table.

Description
-------------------------------------------------

I define the basics methods needed by a FTTreeDataSource. 
I use FTTreeItem to manage my elements and I am use by a FTFastTable.

Public API and Key Messages
-------------------------------------------------

- #data. anObject from: aFTTreeDataSource
	This is my constructor that is use by FTTreeDataSource and myself
	
Example
-------------------------------------------------

Should not be instanciate.
 
Internal Representation and Key Implementation Points.
-------------------------------------------------

    Instance Variables
	dataSource:		I am the dataSource that holds this Item. 
	children:		I am a collection of Items calculate by the item. I contains the chldren of the Item.

