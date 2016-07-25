class
addTraitIn: aPackage
	" add a new class in the provided package. A dialog will be opened to let the user set the class definition "
	|  systemCategory classDefinition newTrait |
	
	systemCategory := aPackage
		ifNotNil: [ aPackage categoryName ]
		ifNil: [ 'Unknown' ].
	classDefinition := self class defineTraitTemplate 
		format: { #TNameOfTrait. '{}'. systemCategory }.
	classDefinition := UIManager default 
		multiLineRequest: 'Define class definition:'
		initialAnswer: classDefinition
		answerHeight: 250.
	(classDefinition isNil or: [ classDefinition isEmpty ])
		ifTrue: [ ^ self ].
	self class evaluate: classDefinition.
	
	newTrait := self class evaluate: classDefinition.
	self browseClass: newTrait.