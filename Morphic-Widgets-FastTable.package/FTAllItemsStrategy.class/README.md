I am a Strategy that will use the function on all the tree.
BE CAREFUL, do NOT use me if you are not sur that your tree is finish. If a branch is infinite I will go in an infinite loop.
 If you are sure about it, use this otherwise use other search strategies. 

Description
-----------------------

I will search in every item of the tree and I needed I will expand the items to show the results of the function.
As said in my superclass I am use by a FTTreeDataSource to help with a FTFunction.

Public API and Key Messages
-----------------------

- #realSearch 	is the method that will launch the search.

Example
-----------------------

	| ds |
	ds := FTTreeDataSource
		roots:
			((ProtoObject allSubclasses sort: [ :a :b | a asString < b asString ])
				reject: [ :e | e asString endsWith: 'class' ])
		children: [ :item | item subclasses sort: [ :a :b | a asString < b asString ] ].
	ds searchStrategy: #allItems. "This will say to the FTTreeDataSource to use me."
	FTTableMorph new
		extent: 200 @ 400;
		dataSource: ds;
		explicitFunction;
		openInWindow
 
Internal Representation and Key Implementation Points.
-----------------------

    Instance Variables
	index:		I am the index of tfe elements I am testing now. 
	result:		I am a collection of index that is the result of the search.
