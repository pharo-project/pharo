Class {
	#name : 'CDDoubleByteClassParserTest',
	#superclass : 'CDClassDefinitionParserTest',
	#category : 'ClassParser-Tests',
	#package : 'ClassParser-Tests'
}

{ #category : 'helpers' }
CDDoubleByteClassParserTest >> classDefinitionString [

	^ '{superclassName} << #{classname}
		layout: DoubleByteLayout;
		slots: {slots};
		sharedVariables: {sharedVariables};
		package: #MyPackage' format: {
			  'classname' -> self className.
			  'superclassName' -> self superclassName.
			  'slots' -> self slotsArray.
			  'sharedVariables' -> self sharedVariablesArray } asDictionary
]

{ #category : 'helpers' }
CDDoubleByteClassParserTest >> testDoubleByteClass [

	self assert: classDefinition layoutClass equals: DoubleByteLayout
]
