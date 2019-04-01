Class {
	#name : #CDEphemeronClassParserTest,
	#superclass : #CDClassDefinitionParserTest,
	#category : #'ClassParser-Tests'
}

{ #category : #helpers }
CDEphemeronClassParserTest >> classDefinitionString [

	^ '{superclassName} ephemeronSubclass: #{classname}
		instanceVariableNames: ''{instvar1} {instvar2}''
		classVariableNames: ''{classvar1} {classvar2}''
		package: #MyPackage'
			format: { 
				'classname' -> self className.
				'superclassName' -> self superclassName.
				'instvar1' -> self firstInstanceVariableName.
				'instvar2' -> self secondInstanceVariableName.
				'classvar1' -> self firstClassVariableName.
				'classvar2' -> self secondClassVariableName. } asDictionary
]

{ #category : #helpers }
CDEphemeronClassParserTest >> testEphemeronClassIsEphemeron [

	self assert: classDefinition isEphemeron
]
