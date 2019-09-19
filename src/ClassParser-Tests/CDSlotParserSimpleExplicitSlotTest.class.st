"
A slot definition can be composed as follows: 

{
#x .
#x => Observable .
#x => (Observable keyword: literal message: literal2)
}
This test is about the second case.
 
"
Class {
	#name : #CDSlotParserSimpleExplicitSlotTest,
	#superclass : #CDClassDefinitionParserTest,
	#category : #'ClassParser-Tests'
}

{ #category : #helpers }
CDSlotParserSimpleExplicitSlotTest >> classDefinitionString [

	^ '{superclassname} subclass: #{classname}
		slots: \{
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
CDSlotParserSimpleExplicitSlotTest >> testClassDefSimpleSlotDefinition [

	" slot: { #instVar3 => LazySlot} "
	self
		assert: classDefinition slotNodes second name
		equals: self secondInstanceVariableName.
	self
		assert: classDefinition slotNodes second slotClassName
		equals: #InstanceVariableSlot.
	self
		assert: classDefinition slotNodes second initializationMessage printString
		equals: ''.
		
	
]
