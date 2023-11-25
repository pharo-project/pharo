"
Tests related to special selector used bu the VM for some callback messages
"
Class {
	#name : #SpecialSelectorTest,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Special'
}

{ #category : #'reflective operations' }
SpecialSelectorTest >> object: anObject perform: aSymbol withArguments: anArray [
	"Helper similar to `Object>>#perform:withArguments:` but that does not send a message to the receiver (because our receiver are quite unreliables)"

	<primitive: 84 error: ec>
	^ #bad -> ec
]

{ #category : #tests }
SpecialSelectorTest >> testAttemptToAssignWithIndex [

	| object |
	object := TestSpecialSelector new.
	"Set the ivar before we are forbideen to assign.
	We will mutate it instead of (attempt) to assign it."
	object lastHook: nil->nil.

	object value: 1.
	self assert: object value equals: 1.
	self assert: object lastHook equals: nil->nil.
	
	object setIsReadOnlyObject: true.
	object value: 20.
	self assert: object value equals: 1.
	self assert: object lastHook key equals: #attemptToAssign:withIndex:.
	self assert: object lastHook value first equals: 20.
	self assert: object lastHook value second equals: 1. "ivar index of value"

	object setIsReadOnlyObject: false.
	object value: 3.
	self assert: object value equals: 3.
]

{ #category : #tests }
SpecialSelectorTest >> testAttemptToAssignWithIndexObject [

	| object |
	object := Association new.

	object value: 1.
	self assert: object value equals: 1.

	object setIsReadOnlyObject: true.
	self should: [ object value: 20 ] raise: ModificationForbidden.

	object setIsReadOnlyObject: false.
	object value: 3.
	self assert: object value equals: 3
]

{ #category : #tests }
SpecialSelectorTest >> testCannotInterpret [

	| class object result |
	"Create a new anonymous subclass of TestSpecialSelector with a nil methodDict"
	class := Behavior new.
	class basicSuperclass: TestSpecialSelector.
	class methodDict: nil.

	"Do not call new, or suffer cannotInterpret initialize"
	object := class basicNew.

	"Any message send to `object` should callback cannotInterpret:"
	result := object max: 42.

	self assert: result key equals: #cannotInterpret:.
	self assert: result value selector equals: #max:.
	self assert: result value arguments equals: #( 42 ).
	self assert: result value lookupClass equals: class
]

{ #category : #tests }
SpecialSelectorTest >> testCannotInterpretPerform [
	"Try to catch `cannotInterpret:` trough a primitive message send"

	| class object result |
	"Create a new anonymous subclass of TestSpecialSelector with a nil methodDict"
	class := Behavior new.
	class basicSuperclass: TestSpecialSelector.
	class methodDict: nil.

	"Do not call new, or suffer cannotInterpret initialize"
	object := class basicNew.

	"Any message send to `object` should callback cannotInterpret:"
	result := self object: object perform: #min: withArguments: #( 42 ).

	self assert: result key equals: #cannotInterpret:.
	self assert: result value selector equals: #min:.
	self assert: result value arguments equals: #( 42 ).
	self assert: result value lookupClass equals: class
]

{ #category : #tests }
SpecialSelectorTest >> testCannotInterpretProtoObject [

	| class object |
	"Create a new anonymous subclass of ProtoObject with a nil methodDict"
	class := Behavior new.
	class basicSuperclass: ProtoObject.
	class methodDict: nil.

	"Do not call new, or suffer cannotInterpret initialize"
	object := class basicNew.

	"Any message send to `object` should callback cannotInterpret: and raise an error"
	self should: [ object max: 42 ] raise: Error
]

{ #category : #tests }
SpecialSelectorTest >> testCannotInterpretProtoObjectGotOne [

	| class object result |
	"Create a new anonymous subclass of ProtoObject with a nil methodDict"
	class := Behavior new.
	class basicSuperclass: ProtoObject.
	class methodDict: nil.

	"Do not call new, or suffer cannotInterpret initialize"
	object := class basicNew.

	"ProtoObject>>cannotInterpret: will find ProtoObject>>class"
	result := object class.

	self assert: result equals: class
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
SpecialSelectorTest >> testMessageNotUnderstoodPerform [

	| object result |
	object := TestSpecialSelector new.

	result := object perform: #max: with: 42.

	self assert: result key equals: #doesNotUnderstand:.
	self assert: result value selector equals: #max:.
	self assert: result value arguments equals: #( 42 ).
	self assert: result value lookupClass equals: TestSpecialSelector
]

{ #category : #tests }
SpecialSelectorTest >> testMessageNotUnderstoodProtoObject [

	| object |
	object := Object new.
	self should: [ object max: 42 ] raise: MessageNotUnderstood
]

{ #category : #tests }
SpecialSelectorTest >> testMustBeBoolean [
	"The returned value of `mustBeBoolean` is unused by the VM.
	 When proceeding, the next instruction seems compiler dependent."

	| object value |
	object := TestSpecialSelector new.
	value := object
		         ifTrue: [ 1 ]
		         ifFalse: [ 10 ].
	self assert: value equals: 1.
	self assert: object lastHook key equals: #mustBeBoolean.

	object := TestSpecialSelector new.
	value := object
		         ifFalse: [ 2 ]
		         ifTrue: [ 20 ].
	self assert: value equals: 2.
	self assert: object lastHook key equals: #mustBeBoolean.

	object := TestSpecialSelector new.
	value := object ifTrue: [ 3 ].
	self assert: value equals: 3.
	self assert: object lastHook key equals: #mustBeBoolean.

	object := TestSpecialSelector new.
	value := object ifFalse: [ 4 ].
	self assert: value equals: nil.
	self assert: object lastHook key equals: #mustBeBoolean.

	object := TestSpecialSelector new.
	value := object and: [ 5 ].
	self assert: value equals: 5.
	self assert: object lastHook key equals: #mustBeBoolean.

	object := TestSpecialSelector new.
	value := true and: [ object ].
	self assert: value equals: object.
	self assert: object lastHook isNil.

	object := TestSpecialSelector new.
	value := object or: [ 6 ].
	self assert: value equals: true.
	self assert: object lastHook key equals: #mustBeBoolean.

	object := TestSpecialSelector new.
	value := nil.
	[ :break |
	[ object ] whileTrue: [
		value := 7.
		break value ] ] valueWithExit.
	self assert: value equals: 7.
	self assert: object lastHook key equals: #mustBeBoolean.

	object := TestSpecialSelector new.
	value := nil.
	[ :break |
	[ object ] whileFalse: [
		value := 8.
		break value ] ] valueWithExit.
	self assert: value equals: 8.
	self assert: object lastHook key equals: #mustBeBoolean
]

{ #category : #tests }
SpecialSelectorTest >> testMustBeBooleanProtoObject [

	| object |
	object := Object new.

	self
		should: [
			object
				ifTrue: [ 1 ]
				ifFalse: [ 2 ] ]
		raise: NonBooleanReceiver
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
