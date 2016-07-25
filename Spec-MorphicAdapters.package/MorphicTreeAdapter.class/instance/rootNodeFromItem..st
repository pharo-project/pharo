widget API
rootNodeFromItem: item
	| node |
	
	node := item isTreeNodeModel
		ifTrue: [ item ]
		ifFalse: [ self rootNodeHolder cull: item ].
		
	node container: self model.
	^ node buildWithSpec