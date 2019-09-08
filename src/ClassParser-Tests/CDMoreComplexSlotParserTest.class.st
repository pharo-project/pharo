Class {
	#name : #CDMoreComplexSlotParserTest,
	#superclass : #CDClassDefinitionParserTest,
	#category : #'ClassParser-Tests'
}

{ #category : #helpers }
CDMoreComplexSlotParserTest >> classDefinitionString [

	^ '{superclassname} subclass: #{classname}
		slots: \{
		''{instvar1}'' =>  LazyClassVariable default: 5. 
		''{instvar2}'' => InstanceVariableSlot.
		''instVar3''
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

{ #category : #helpers }
CDMoreComplexSlotParserTest >> testClassDefFromLegacyStringHasSlots [

	self assert: classDefinition slotNodes first name equals: self firstInstanceVariableName.
	self assert: classDefinition slotNodes first typeName equals: #LazyClassVariable.
	self assert: classDefinition slotNodes first initializationMessage equals: 'default: 5'.

	self assert: classDefinition slotNodes second name equals: self secondInstanceVariableName.
	self assert: classDefinition slotNodes second typeName equals: #InstanceVariableSlot.
	
	self assert: classDefinition slotNodes third name equals: 'instVar3'
	

]
