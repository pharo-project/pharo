Class {
	#name : 'SuperVariableTest',
	#superclass : 'TestCase',
	#category : 'Slot-Tests-VariablesAndSlots',
	#package : 'Slot-Tests',
	#tag : 'VariablesAndSlots'
}

{ #category : 'tests' }
SuperVariableTest >> testReadInContext [

	| var |
	var := self class lookupVar: #super.
	self assert: (var readInContext: thisContext) identicalTo: super.
	"read from a block context"
	self assert: [(var readInContext: thisContext)] value identicalTo: super
]

{ #category : 'tests' }
SuperVariableTest >> testReadInContextClean [
	<compilerOptions: #( +optionCleanBlockClosure)>
	| var block |
	"if context is one stack we can read"
	block := [ (thisContext lookupVar: #super) readInContext: thisContext ].
	self assert: block value equals: super.
	"but no chance if not, then it is nil"
	var := self class lookupVar: #self.
	block :=  [ thisContext ].
	self assert: (var readInContext: block value ) equals: nil
]

{ #category : 'tests' }
SuperVariableTest >> testUsingMethods [

	|  var |
	var := super class lookupVar: #super.
	self assert: (var usingMethods includes: thisContext method)
]
