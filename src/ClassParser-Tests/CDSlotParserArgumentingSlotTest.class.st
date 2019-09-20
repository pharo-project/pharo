"
A slot definition can be composed as follows: 

{
#x .
#x => Observable .
#x => (Observable keyword: literal message: literal2)
}
This test is about the second case.


However I'm not really happy about the representation of literal in the RBSlotInitializationNode class.
Indeed I currently store '5' as argument of keyword: 5 and probably 'true' for keyword: true
and it will force the visitors to rescan. 
"
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
		''{instvar2}'' =>  (ObservableSlot default: 5). 
		\}
		classVariableNames: ''{classvar1} {classvar2}''
		package: #MyPackage'
			format: { 
				'classname' -> self className.
				'superclassname' -> self superclassName.
				'instvar1' -> self firstInstanceVariableName.
				'instvar2' -> self secondInstanceVariableName.
				'classvar1' -> self firstClassVariableName.
				'classvar2' -> self secondClassVariableName } asDictionary
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
		equals: 'default: ''5'''.
		
	
]
