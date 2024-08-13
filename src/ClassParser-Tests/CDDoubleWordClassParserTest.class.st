Class {
	#name : 'CDDoubleWordClassParserTest',
	#superclass : 'CDClassDefinitionParserTest',
	#category : 'ClassParser-Tests',
	#package : 'ClassParser-Tests'
}

{ #category : 'helpers' }
CDDoubleWordClassParserTest >> classDefinitionString [

	^ '{superclassName} << #{classname}
		layout: DoubleWordLayout;
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
CDDoubleWordClassParserTest >> testDoubleWordClass [

	self assert: classDefinition layoutClass equals: DoubleWordLayout
]
