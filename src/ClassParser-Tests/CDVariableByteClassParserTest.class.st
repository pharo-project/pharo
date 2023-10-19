Class {
	#name : 'CDVariableByteClassParserTest',
	#superclass : 'CDClassDefinitionParserTest',
	#category : 'ClassParser-Tests',
	#package : 'ClassParser-Tests'
}

{ #category : 'helpers' }
CDVariableByteClassParserTest >> classDefinitionString [

	^ '{superclassName} << #{classname}
		layout: ByteLayout;
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
CDVariableByteClassParserTest >> testVariableByteClassIsBytes [

	self assert: classDefinition layoutClass equals: ByteLayout
]
