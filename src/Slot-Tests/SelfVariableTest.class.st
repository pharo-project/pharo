Class {
	#name : #SelfVariableTest,
	#superclass : #TestCase,
	#category : #'Slot-Tests-VariablesAndSlots'
}

{ #category : #tests }
SelfVariableTest >> testUsingMethods [

	|  var |
	var := self class lookupVar: #self.
	self assert: (var usingMethods includes: thisContext method)
]

{ #category : #tests }
SelfVariableTest >> testUsingMethodsFFI [

	|  var |
	var := self class lookupVar: #self.

	"We find an ffi method that was run and thus does not contain any push self, yet has self send in the code"
	self assert: (var usingMethods anySatisfy: [:method | method isFFIMethod and: [ method readsSelf not ]])
]

{ #category : #tests }
SelfVariableTest >> testUsingMethodsSuper [

	|  var |
	"we use super instead of self to check that super sends are not understood as self sends"
	var := super class lookupVar: #self.
	super deny: (var usingMethods includes: thisContext method)
]
