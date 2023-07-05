"
Tests related to special selector used bu the VM for some callback messages
"
Class {
	#name : #SpecialSelectorTest,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Special'
}

{ #category : #tests }
SpecialSelectorTest >> testCannotInterpret [
	"Unfortunately, cannotInterpret: is currently broken.
	The result of `cannotInterpret:` seems to be used as a receiver for the original message AGAIN.
	This is likely a VM bug :("

	| class object result |
	"Create a new anonymous subclass of TestSpecialSelector with a nil methodDict"
	class := Behavior new.
	class basicSuperclass: TestSpecialSelector.
	class methodDict: nil.
	object := class new.
	"Any message send to `object` should callback cannotInterpret:"
	result := object max: 42.
	self assert: result key equals: #cannotInterpret:.
	self assert: result value selector equals: #max:.
	self assert: result value arguments equals: #( 42 ).
	self assert: result value lookupClass equals: TestSpecialSelector
]

{ #category : #tests }
SpecialSelectorTest >> testCannotInterpretProtoObject [
	"Test the implementation of ProtoObject>>cannotInterpret:
	Is currenlty broken and goes in an infinite recursion of Context>>nextHandlerContext"

	| class object |
	"Create a new anonymous subclass of ProtoObject with a nil methodDict"
	class := Behavior new.
	class basicSuperclass: ProtoObject.
	class methodDict: nil.
	object := class new.
	"Any message send to `object` should callback cannotInterpret: and raise an error"
	self should: [ object max: 42 ] raise: Error
]

{ #category : #tests }
SpecialSelectorTest >> testMessageNotUnderstood [

	| object result |
	object := TestSpecialSelector new.
	result := object max: 42.
	self assert: result key equals: #doesNotUnderstand:.
	self assert: result value selector equals: #max:.
	self assert: result value arguments equals: #( 42 ).
	self assert: result value lookupClass equals: TestSpecialSelector
]

{ #category : #tests }
SpecialSelectorTest >> testMustBeBoolean [
	"The returned value of `mustBeBoolean` seems unused by the VM. Weird"

	| object value |
	object := TestSpecialSelector new.

	object value: true.
	value := object
		         ifTrue: [ 1 ]
		         ifFalse: [ 2 ].
	self assert: value equals: 1.
	self assert: object lastHook key equals: #mustBeBoolean.

	object value: false.
	value := object or: [ object ].
	self assert: value equals: true.
	self assert: object lastHook key equals: #mustBeBoolean
]

{ #category : #tests }
SpecialSelectorTest >> testRunWithIn [

	| selector method object result |
	selector := #hookedMethod:.
	method := TestSpecialMethodObject new.
	method selector: #hookedMethod:.
	method realMethod: TestSpecialSelector>>#foo:.
	TestSpecialSelector addSelector: selector withMethod: method.
	object := TestSpecialSelector new.
	result := object hookedMethod: 42.
	TestSpecialSelector removeSelector: selector.

	self assert: result key equals: #run:with:in:.
	self assert: result value first equals: selector.
	self assert: result value second equals: #( 42 ).
	self assert: result value third equals: object
]
