private
moveElementAt: index to: newIndex
	| elementToMove orderedList |
	
	(newIndex < 1 or: [ newIndex > list listItems size ]) ifTrue: [ ^self ].
	
	elementToMove := list listItems at: index.	
	orderedList := list listItems asOrderedCollection
		removeAt: index;
		add: elementToMove beforeIndex: newIndex;
		yourself.
	self list: orderedList.
	self list setSelectedIndex: newIndex.