I am a Strategy that will use the function on the visible items of the tree.

Description
-----------------------

I will search in the vsible items of the tree.
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
	ds searchStrategy: #default. "This will say to the FTTreeDataSource to use me, but since this is the default, this is optional."
	FTTableMorph new
		extent: 200 @ 400;
		dataSource: ds;
		explicitFunction;
		openInWindow
 