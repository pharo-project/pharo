Class {
	#name : 'CDVariableWordClassParserTest',
	#superclass : 'CDClassDefinitionParserTest',
	#category : 'ClassParser-Tests',
	#package : 'ClassParser-Tests'
}

{ #category : 'helpers' }
CDVariableWordClassParserTest >> classDefinitionString [

	^ '{superclassName} << #{classname}
		layout: WordLayout;
		slots: {slots};
		sharedVariables: {sharedVariables};
		package: #MyPackage'
			format: {
				'classname' -> self className.
				'superclassName' -> self superclassName.
				'slots' -> self slotsArray.
				'sharedVariables' -> self sharedVariablesArray } asDictionary
]

{ #category : 'helpers' }
CDVariableWordClassParserTest >> testVariableWordClassIsWords [

	self assert: classDefinition layoutClass equals: WordLayout
]
