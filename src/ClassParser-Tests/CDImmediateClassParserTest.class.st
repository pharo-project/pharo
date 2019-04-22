Class {
	#name : #CDImmediateClassParserTest,
	#superclass : #CDClassDefinitionParserTest,
	#category : #'ClassParser-Tests'
}

{ #category : #helpers }
CDImmediateClassParserTest >> classDefinitionString [

	^ '{superclassName} immediateSubclass: #{classname}
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
CDImmediateClassParserTest >> testImmediateClassIsImmediate [

	self assert: classDefinition isImmediate
]
