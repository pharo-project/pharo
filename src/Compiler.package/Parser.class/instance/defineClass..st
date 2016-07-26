error correction
defineClass: className 
	"Prompts the user to define a new class."
	
	| classSymbol systemCategory classDefinition |
	classSymbol := className asSymbol.
	systemCategory := self encoder classEncoding theNonMetaClass category
		ifNil: [ 'Unknown' ].
	classDefinition := 'Object subclass: #' , classSymbol , '
		instanceVariableNames: '''' 
		classVariableNames: ''''
		poolDictionaries: ''''
		category: ''' , systemCategory , ''''.
	classDefinition := UIManager default 
		multiLineRequest: 'Edit class definition:'
		initialAnswer: classDefinition
		answerHeight: 150.
	(classDefinition isNil or: [ classDefinition isEmpty ])
		ifTrue: [ ^ self fail ].
	self class compiler evaluate: classDefinition.
	^ encoder
		global: (Smalltalk globals associationAt: classSymbol)
		name: classSymbol