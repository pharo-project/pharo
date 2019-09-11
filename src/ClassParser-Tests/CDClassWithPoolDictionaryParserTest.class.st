Class {
	#name : #CDClassWithPoolDictionaryParserTest,
	#superclass : #CDClassDefinitionParserTest,
	#category : #'ClassParser-Tests'
}

{ #category : #helpers }
CDClassWithPoolDictionaryParserTest >> classDefinitionString [
	^ '{superclassName} subclass: #{classname}
		instanceVariableNames: ''{instvar1} {instvar2}''
		classVariableNames: ''{classvar1} {classvar2}''
		poolDictionaries: ''{sharedPoolName}''
		package: #MyPackage'
			format: { 
				'classname' -> self className.
				'superclassName' -> self superclassName.
				'instvar1' -> self firstInstanceVariableName.
				'instvar2' -> self secondInstanceVariableName.
				'classvar1' -> self firstClassVariableName.
				'classvar2' -> self secondClassVariableName. 
				'sharedPoolName' -> self sharedPoolName } asDictionary
]

{ #category : #helpers }
CDClassWithPoolDictionaryParserTest >> sharedPoolName [
	^ 'TextConstants AnotherSharedPool'
]
