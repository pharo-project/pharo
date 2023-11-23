Class {
	#name : 'CDEphemeronClassParserTest',
	#superclass : 'CDClassDefinitionParserTest',
	#category : 'ClassParser-Tests',
	#package : 'ClassParser-Tests'
}

{ #category : 'helpers' }
CDEphemeronClassParserTest >> classDefinitionString [

	^ '{superclassName} << #{classname}
		layout: EphemeronLayout;
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
CDEphemeronClassParserTest >> testEphemeronClassIsEphemeron [

	self assert: classDefinition layoutClass equals: EphemeronLayout
]
