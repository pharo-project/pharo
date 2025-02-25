Extension { #name : 'ClassDescription' }

{ #category : '*ClassDefinitionPrinters' }
ClassDescription >> definitionPrinter [
	"Return a configurated printer associated with the current class definition format."

	^ ClassDefinitionPrinter for: self
]

{ #category : '*ClassDefinitionPrinters' }
ClassDescription >> definitionString [
	"Return the string of the class definition, each of my subclass may tell the printer how to printer it. A kind of double dispatch since we have multiple printers and multiple entities to be printed."

	^ self definitionPrinter definitionString
]

{ #category : '*ClassDefinitionPrinters' }
ClassDescription >> definitionStringFor: aConfiguredPrinter [

	^ self subclassResponsibility
]

{ #category : '*ClassDefinitionPrinters' }
ClassDescription >> expandedDefinitionString [
	"Return the string of the class definition, each of my subclass may tell the printer how to printer it. A kind of double dispatch since we have multiple printers and multiple entities to be printed."

	^ self expandedDefinitionStringFor: (self definitionPrinter for: self)
]

{ #category : '*ClassDefinitionPrinters' }
ClassDescription >> expandedDefinitionStringFor: aPrinter [

	^ self subclassResponsibility
]

{ #category : '*ClassDefinitionPrinters' }
ClassDescription >> oldDefinition [

	^ ClassDefinitionPrinter legacy
		for: self;
		definitionString
]

{ #category : '*ClassDefinitionPrinters' }
ClassDescription >> oldPharoDefinition [

	^ ClassDefinitionPrinter oldPharo
		for: self;
		definitionString
]
