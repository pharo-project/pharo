correcting
defineClass: className 
	"Prompts the user to define a new class."
	
	| classSymbol systemCategory classDefinition |
	classSymbol := className asSymbol.
	systemCategory := self methodClass category
		ifNil: [ 'Unknown' ].
	classDefinition := 'Object subclass: #' , classSymbol , '
		instanceVariableNames: '''' 
		classVariableNames: ''''
		category: ''' , systemCategory , ''''.
	classDefinition := UIManager default 
		multiLineRequest: 'Edit class definition:'
		initialAnswer: classDefinition
		answerHeight: 150.
	(classDefinition isNil or: [ classDefinition isEmpty ])
		ifTrue: [ ^ Abort signal ].
	self class compiler
		source: classDefinition;
		logged: true;
		evaluate.
	^ (node owningScope lookupVar: className)
		ifNil: [self error: 'should be not happen']