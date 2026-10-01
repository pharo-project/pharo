"
Test for class variables
"
Class {
	#name : 'ExampleClassVariableWithStateTest',
	#superclass : 'SlotSilentTest',
	#category : 'Slot-Examples-Tests-ClassVariables',
	#package : 'Slot-Examples-Tests',
	#tag : 'ClassVariables'
}

{ #category : 'tests' }
ExampleClassVariableWithStateTest >> testCreateClassWithClassVariable [
	"Add class variable using the builder interface"

	| classVar |

	classVar := #ClassVar => ExampleClassVariableWithState.

	aClass := self make: [ :builder |
		builder
			sharedVariables: {classVar}
		].

	self assert: (aClass hasClassVarNamed: 'ClassVar')
]

{ #category : 'tests' }
ExampleClassVariableWithStateTest >> testCreateClassWithTwoClassVariable [
	"Add class variable using the builder interface"

	| classVar1 classVar2 |

	classVar1 := #ClassVar1 => ExampleClassVariableWithState.
	classVar2 := #ClassVar2 => ExampleClassVariableWithState.

	aClass := self make: [ :builder |
		builder
			sharedVariables: {classVar1. classVar2}
		].

	self assert: (aClass hasClassVarNamed: 'ClassVar1').
	self assert: (aClass hasClassVarNamed: 'ClassVar2')
]

{ #category : 'tests' }
ExampleClassVariableWithStateTest >> testMigrateClassVar [
	"Add class variable using the builder interface and change it to another kind"

	| classVar |

	classVar := #ClassVar => ExampleClassVariableWithState.

	aClass := self make: [ :builder |
		builder
			sharedVariables: {#ClassVar}
		].

	self assert: (aClass hasClassVarNamed: 'ClassVar').
	self assert: (aClass classVariableNamed: #ClassVar) class equals: ClassVariable.

	aClass writeClassVariableNamed: 'ClassVar' value: 5.

	aClass := self make: [ :builder |
		builder
			sharedVariables: {classVar}
		].

	self assert: (aClass hasClassVarNamed: 'ClassVar').
	self assert: (aClass classVariableNamed: #ClassVar) class equals: ExampleClassVariableWithState.
	self assert: (aClass classVariableNamed: #ClassVar) read equals: 5.
	self assert: (aClass classVariableNamed: #ClassVar) definingClass equals: aClass.
	self assert: (aClass classVariableNamed: #ClassVar) owningClass equals: aClass
]
