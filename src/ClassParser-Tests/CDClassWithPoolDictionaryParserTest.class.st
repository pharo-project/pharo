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
		sharedVariables: \{ #{classvar1} . #{classvar2} };
		sharedPools: {sharedPoolName}
		package: #MyPackage' format: {
			  'classname' -> self className.
			  'superclassName' -> self superclassName.
			  'slots' -> self slotsArray.
			  'classvar1' -> self firstClassVariableName.
			  'classvar2' -> self secondClassVariableName.
			  'sharedPoolName' -> self sharedPools } asDictionary
]

{ #category : 'helpers' }
CDClassWithPoolDictionaryParserTest >> sharedPools [

	^ '{ TextConstants. AnotherSharedPool }'
]
