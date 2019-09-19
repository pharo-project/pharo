"
A slot definition can be composed as follows: 

{
#x .
#x => Observable .
#x => (Observable keyword: literal message: literal2)
}

This tests is about the first case.
"
Class {
	#name : #CDSlotParserSimpleInstanceVariableTest,
	#superclass : #CDClassDefinitionParserTest,
	#category : #'ClassParser-Tests'
}

{ #category : #helpers }
CDSlotParserSimpleInstanceVariableTest >> classDefinitionString [

	^ '{superclassname} subclass: #{classname}
		slots: \{
		''{instvar1}'' .
		''{instvar2}''
		\}
		classVariableNames: ''{classvar1} {classvar2}''
		package: #MyPackage'
			format: { 
				'classname' -> self className.
				'superclassname' -> self superclassName.
				'instvar1' -> self firstInstanceVariableName.
				'instvar2' -> self secondInstanceVariableName.
				'classvar1' -> self firstClassVariableName.
				'classvar2' -> self secondClassVariableName 
		} asDictionary
]

{ #category : #tests }
CDSlotParserSimpleInstanceVariableTest >> testClassDefFromLegacyString [

	" slot: { #instVar3} "

	self assert: classDefinition slotNodes first name equals: 'first'.
	self
		assert: classDefinition slotNodes first slotClassName
		equals: #InstanceVariableSlot.
	self
		assert: classDefinition slotNodes first initializationMessage printString
		equals: ''.
]
