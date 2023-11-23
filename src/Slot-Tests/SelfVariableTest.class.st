Class {
	#name : 'SelfVariableTest',
	#superclass : 'TestCase',
	#category : 'Slot-Tests-VariablesAndSlots',
	#package : 'Slot-Tests',
	#tag : 'VariablesAndSlots'
}

{ #category : 'tests' }
SelfVariableTest >> testReadInContext [

	| var |
	var := self class lookupVar: #self.
	self assert: (var readInContext: thisContext) identicalTo: self.
	"read from a block context"
	self assert: [(var readInContext: thisContext)] value identicalTo: self
]

{ #category : 'tests' }
SelfVariableTest >> testReadInContextClean [
	<compilerOptions: #( +optionCleanBlockClosure)>
	| var block |
	"if context is one stack we can read"
	block := [ (thisContext lookupVar: #self) readInContext: thisContext ].
	self assert: block value equals: self.
	"but no chance if not, then it is nil"
	var := self class lookupVar: #self.
	block :=  [ thisContext ].
	self assert: (var readInContext: block value ) equals: nil
]

{ #category : 'tests' }
SelfVariableTest >> testUsingMethods [

	|  var |
	var := self class lookupVar: #self.
	self assert: (var usingMethods includes: thisContext method)
]

{ #category : 'tests' }
SelfVariableTest >> testUsingMethodsFFI [

	|  var |
	var := self class lookupVar: #self.

	"We find an ffi method that was run and thus does not contain any push self, yet has self send in the code"
	self assert: (var usingMethods anySatisfy: [:method | method isFFIMethod and: [ method readsSelf not ]])
]

{ #category : 'tests' }
SelfVariableTest >> testUsingMethodsSuper [

	|  var |
	"we use super instead of self to check that super sends are not understood as self sends"
	var := super class lookupVar: #self.
	super deny: (var usingMethods includes: thisContext method)
]
