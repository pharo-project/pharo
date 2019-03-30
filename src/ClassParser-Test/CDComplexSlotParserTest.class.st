Class {
	#name : #CDComplexSlotParserTest,
	#superclass : #CDClassDefinitionParserTest,
	#category : #'ClassParser-Test'
}

{ #category : #helpers }
CDComplexSlotParserTest >> classDefinitionString [

	^ '{superclassname} subclass: #{classname}
		slots: \{
		''{instvar1}'' => InstanceVariableSlot. 
		''{instvar2}'' => InstanceVariableSlot.
		\}
		classVariableNames: ''{classvar1} {classvar2}''
		package: #MyPackage'
			format: { 
				'classname' -> self className.
				'superclassname' -> self superclassName.
				'instvar1' -> self firstInstanceVariableName.
				'instvar2' -> self secondInstanceVariableName.
				'classvar1' -> self firstClassVariableName.
				'classvar2' -> self secondClassVariableName. } asDictionary
]
