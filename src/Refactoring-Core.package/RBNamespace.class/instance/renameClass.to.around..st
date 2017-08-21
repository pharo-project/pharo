changes
renameClass: aRBClass to: aSymbol around: aBlock
	| change value dict |
	change := RBRefactoryChangeManager changeFactory renameClass: aRBClass to: aSymbol.
	self performChange: change around: aBlock.
	self flushCaches.
	dict := (newClasses includesKey: aRBClass name)
		ifTrue: [ newClasses ]
		ifFalse: [ changedClasses ].
	self markAsRemoved: aRBClass name.
	self unmarkAsRemoved: aSymbol.
	value := dict at: aRBClass name.
	dict removeKey: aRBClass name.
	dict at: aSymbol put: value.
	value first name: aSymbol.
	value last name: aSymbol.
	value first subclasses do: [ :each | each superclass: value first ].
	value last subclasses do: [ :each | each superclass: value last ].
	^ change