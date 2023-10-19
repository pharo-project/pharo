Class {
	#name : 'CDWeakClassParserTest',
	#superclass : 'CDClassDefinitionParserTest',
	#category : 'ClassParser-Tests',
	#package : 'ClassParser-Tests'
}

{ #category : 'helpers' }
CDWeakClassParserTest >> classDefinitionString [

	^ '{superclassName} weakSubclass: #{classname}
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
CDWeakClassParserTest >> testWeakClassIsWeak [

	self assert: classDefinition layoutClass equals: WeakLayout
]
