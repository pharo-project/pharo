Class {
	#name : #CDSlotParserArgumentingSlotTest,
	#superclass : #CDClassDefinitionParserTest,
	#category : #'ClassParser-Tests'
}

{ #category : #helpers }
CDSlotParserArgumentingSlotTest >> classDefinitionString [

	^ '{superclassname} subclass: #{classname}
		slots: \{
		''{instvar1}'' =>  (LazyClassVariable default: 5). 
		\}
		classVariableNames: ''''
		package: #MyPackage'
			format: { 
				'classname' -> self className.
				'superclassname' -> self superclassName.
				'instvar1' -> self firstInstanceVariableName.
				'instvar2' -> self secondInstanceVariableName } asDictionary
]

{ #category : #tests }
CDSlotParserArgumentingSlotTest >> testClassDefWithInitializationSlot [
	self
		assert: classDefinition slotNodes first name
		equals: self firstInstanceVariableName.
	self
		assert: classDefinition slotNodes first slotClassName
		equals: #LazyClassVariable.
	self
		assert: classDefinition slotNodes first initializationMessage printString
		equals: 'default: 5'.
		
	
]
