Class {
	#name : #ThisProcessVariableTest,
	#superclass : #TestCase,
	#category : #'Slot-Tests-VariablesAndSlots'
}

{ #category : #tests }
ThisProcessVariableTest >> testReadInContext [

	| var |
	var := self class lookupVar: #thisProcess.
	self assert: (var readInContext: thisContext) identicalTo: thisProcess
]
