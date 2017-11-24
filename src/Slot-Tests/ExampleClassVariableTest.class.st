Class {
	#name : #ExampleClassVariableTest,
	#superclass : #SlotSilentTest,
	#category : #'Slot-Tests-Examples'
}

{ #category : #tests }
ExampleClassVariableTest >> testCreateClassWithClassVariable [
	"Add class variable using the builder interface"
	
	| classVar |
	
	classVar := #ClassVar => ExampleClassVariable.
	
	aClass := self make: [ :builder |
		builder 
			sharedVariables: {classVar}
		].
	
	self assert: (aClass hasClassVarNamed: 'ClassVar').



]

{ #category : #tests }
ExampleClassVariableTest >> testCreateClassWithTwoClassVariable [
	"Add class variable using the builder interface"
	
	| classVar1 classVar2 |
	
	classVar1 := #ClassVar1 => ExampleClassVariable.
	classVar2 := #ClassVar2 => ExampleClassVariable.
	
	aClass := self make: [ :builder |
		builder 
			sharedVariables: {classVar1. classVar2}
		].
	
	self assert: (aClass hasClassVarNamed: 'ClassVar1').
	self assert: (aClass hasClassVarNamed: 'ClassVar2').



]

{ #category : #tests }
ExampleClassVariableTest >> testMigrateClassVar [
	"Add class variable using the builder interface and change it to another kind"
	
	| classVar |
	
	classVar := #ClassVar => ExampleClassVariable.
	
	aClass := self make: [ :builder |
		builder 
			sharedVariables: {#ClassVar}
		].
	
	self assert: (aClass hasClassVarNamed: 'ClassVar').
	self assert: (aClass classVariableNamed: #ClassVar) class = ClassVariable.
	
	aClass classVarNamed: 'ClassVar' put: 5.
	
	aClass := self make: [ :builder |
		builder 
			sharedVariables: {classVar}
		].
	
	self assert: (aClass hasClassVarNamed: 'ClassVar').
	self assert: (aClass classVariableNamed: #ClassVar) class = ExampleClassVariable.
	self assert: (aClass classVariableNamed: #ClassVar) read = 5.
]
