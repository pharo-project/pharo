I am an Item of a tree that keep a data and know how to calculate the children of this data for a Fast Tree.

Description
-------------------------------------------------

I am created by a FTTreeDataSource or a FTItem and I hold a data. I can use a childrenBlock from my dataSource to generate the children of my data.
Myself I use some FTBasicItems to creale my children.
I also know how to generate a button for a FTTreeDataSource if needed. This button can be use to extand or unextand me.

Public API and Key Messages
-------------------------------------------------

- #expand/#unexpand
        Allow to expand me or unexpand me and update the Tree.. 
   
- #depth
        Return my depth in the tree.

Create me as as my superclass with an object as data.

Example
-------------------------------------------------

(FTBasicItem data: Object from: (FTTreeDataSource roots: {} children: [ :item | item subclasses ]))
		depth: 2;
		yourself
 
Internal Representation and Key Implementation Points.
-------------------------------------------------

    Instance Variables
	data:		I am an object hold by the item.
	depth:		I am the depth of the Item on the tree.
	isExpanded:		I am a Boolean that remember if I am expanded or not.
	recentlyChanged: 		I am a boolean that return true fi the item was really recently collapsed/expanded. Don't play with me, I am use to update the selection when we collapse/expand an item.
			
I calculate my children with a block that is inside my dataSource. I execute this block with my data, the roots items of the dataSource and my level as arguments.