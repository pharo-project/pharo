Class {
	#name : 'CDImmediateClassParserTest',
	#superclass : 'CDClassDefinitionParserTest',
	#category : 'ClassParser-Tests',
	#package : 'ClassParser-Tests'
}

{ #category : 'helpers' }
CDImmediateClassParserTest >> classDefinitionString [

	^ '{superclassName} << #{classname}
		layout: ImmediateLayout;
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
CDImmediateClassParserTest >> testImmediateClassIsImmediate [

	self assert: classDefinition layoutClass equals: ImmediateLayout
]
