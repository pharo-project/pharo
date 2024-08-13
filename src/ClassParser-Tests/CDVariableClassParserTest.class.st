Class {
	#name : 'CDVariableClassParserTest',
	#superclass : 'CDClassDefinitionParserTest',
	#category : 'ClassParser-Tests',
	#package : 'ClassParser-Tests'
}

{ #category : 'helpers' }
CDVariableClassParserTest >> classDefinitionString [

	^ '{superclassName} << #{classname}
		layout: VariableLayout;
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
CDVariableClassParserTest >> testVariableClassIsVariable [

	self assert: classDefinition layoutClass equals: VariableLayout
]
