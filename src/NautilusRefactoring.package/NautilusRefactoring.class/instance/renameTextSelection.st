source
renameTextSelection
	self model selectedMethod ifNotNil: [ ^ self renameTextSelectionForMethod ].
	(self model selectedCategory isNil and: [ self model selectedClass notNil ])
		ifTrue: [ ^ self renameTextSelectionForClass ]	" we are editing a class definition "