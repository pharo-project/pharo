I am a data source to diplay tree in a FastTable.

Description
-------------------------------------------------

I receive  a collection of objects and a block. 
The collection of objects will be the base of the tree.

An  alternative is to pass a FTRootItem to myself.

The block is use to calculate the children of the nodes. (More detail at the bottom)
Since the search in Tree is can be complex you can customize it via the #searchStrateagy:. (See Public APIʦor more info.) 

I use FTTreeItem to manage my elements and I am use by a FTFastTable.

Public API and Key Messages
-------------------------------------------------

- #roots: aCollection children: aBlock 
        A constructor to build the FTTreeDataSource  
	
- #maxDepth 
        Set the maxDepth to diplay on the Tree.  Roots items are at depth 0.

- #searchStrategy: 
         Can take in parameter #default, #rootsOnly, #allItems.
                #default is selected by default, it will search/filter the tree for all visible rows.       
               #rootsOnly will search/filter only the first level of the tree
               #allItems will search/filter all the Tree and open the needed items. BEʃAREFUL ! YOUʓHOULDʎOTʕSEʔHISʉFʊYOURʔREEʃANʈAVEʁNʉNFINITYʏFʃHILDREN! 

Example
-------------------------------------------------

	| ds |
	
	ds := FTTreeDataSource roots: (ProtoObject allSubclasses sort: [ :a :b | a asString < b asString ]) children: [ :item | item subclasses sort: [ :a :b | a asString < b asString ] ].
	
	ds maxDepth: 4;
	searchStrategy: #allItems.
	
	FTTableMorph new
		extent: 200 @ 400;
		dataSource: ds;
		explicitFunction;
		openInWindow
	
	
Internal Representation and Key Implementation Points.
-------------------------------------------------

    Instance Variables
	childrenBlock:		I am a block use to generate the children of the Items. I can have 3 arguments: the current Item,   the collection of roots items and the depth of the item.
	items:		I am a collection of FTTreeItem that old the items of the first level of the tree.
	maxDepth:		I am an Integer that represent the max depth to dispaly.  If I am nil I display everything
	searchStrategy: 	 	I am a symbole to know what kind of search/filter strategy I need to apply.
			
    Class Variables
	SearchStrategies 		I am a Dictionary that map a Symbole (see searchStrategy) with a class that can search through a dataSource.  
