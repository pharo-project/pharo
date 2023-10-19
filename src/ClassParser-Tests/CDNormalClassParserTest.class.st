Class {
	#name : 'CDNormalClassParserTest',
	#superclass : 'CDClassDefinitionParserTest',
	#category : 'ClassParser-Tests',
	#package : 'ClassParser-Tests'
}

{ #category : 'helpers' }
CDNormalClassParserTest >> classDefinitionString [

	^ '{superclassName} << #{classname}
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
CDNormalClassParserTest >> testNormalClassHasFixedLayout [

	self assert: classDefinition layoutClass equals: FixedLayout
]
