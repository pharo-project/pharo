I am an Item of a tree that keep a data and I keep staticly my children. I cannot regenerate them. 

Description
-------------------------------------------------

I am created by a FTTreeFunctionStrategy for some filter. For example it is hard to be able to update a Tree when you filter all his elements. In that case the FTAllItemsStrategy create me during a filter.

Public API and Key Messages
-------------------------------------------------

- #children: 	allow to set my childrens 
   
Example
-------------------------------------------------

FTStaticBasicItem new
		data: 5;
		depth: 2;
		children: aCollectionOfItems;
		yourself.
 