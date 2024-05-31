Class {
	#name : 'CDClassWithPoolDictionaryParserTest',
	#superclass : 'CDClassDefinitionParserTest',
	#category : 'ClassParser-Tests',
	#package : 'ClassParser-Tests'
}

{ #category : 'helpers' }
CDClassWithPoolDictionaryParserTest >> classDefinitionString [

	^ '{superclassName} << #{classname}
		slots: {slots};
		sharedVariables: {sharedVariables};
		sharedPools: {sharedPoolName}
		package: #MyPackage' format: {
			  'classname' -> self className.
			  'superclassName' -> self superclassName.
			  'slots' -> self slotsArray.
			  'sharedVariables' -> self sharedVariablesArray.
			  'sharedPoolName' -> self sharedPools } asDictionary
]

{ #category : 'helpers' }
CDClassWithPoolDictionaryParserTest >> sharedPools [

	^ '{ TextConstants. AnotherSharedPool }'
]
