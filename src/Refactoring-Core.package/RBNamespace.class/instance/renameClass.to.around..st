changes
renameClass: aRBClass to: aSymbol around: aBlock 
	| change value dict |
	change := RBRenameClassChange rename: aRBClass name to: aSymbol.
	self performChange: change around: aBlock.
	self flushCaches.
	dict := (newClasses includesKey: aRBClass name) 
				ifTrue: [newClasses]
				ifFalse: [changedClasses].
	removedClasses
		add: aRBClass name;
		add: aRBClass name , ' class'.
	value := dict at: aRBClass name.
	dict removeKey: aRBClass name.
	dict at: aSymbol put: value.
	value first name: aSymbol.
	value last name: aSymbol.
	value first subclasses do: [:each | each superclass: value first].
	value last subclasses do: [:each | each superclass: value last].
	^change