Class {
	#name : #ThisContextVariableTest,
	#superclass : #TestCase,
	#category : #'Slot-Tests-VariablesAndSlots'
}

{ #category : #tests }
ThisContextVariableTest >> testReadInContext [

	| var |
	var := self class lookupVar: #thisContext.
	self assert: (var readInContext: thisContext) identicalTo: thisContext
]

{ #category : #tests }
ThisContextVariableTest >> testUsingMethods [

	|  var |
	var := self class lookupVar: #thisContext.
	self assert: (var usingMethods includes: thisContext method)
]
