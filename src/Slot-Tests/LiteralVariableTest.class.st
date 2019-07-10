Class {
	#name : #LiteralVariableTest,
	#superclass : #TestCase,
	#classVars : [
		'TestVariable'
	],
	#category : #'Slot-Tests-VariablesAndSlots'
}

{ #category : #tests }
LiteralVariableTest >> testIsReferenced [
	self assert: (SmalltalkImage classVariableNamed: #CompilerClass) isReferenced
]
