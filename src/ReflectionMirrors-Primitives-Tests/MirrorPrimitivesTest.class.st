"
Unit tests for mirror primitives.
"
Class {
	#name : #MirrorPrimitivesTest,
	#superclass : #TestCase,
	#category : #'ReflectionMirrors-Primitives-Tests-Base'
}

{ #category : #'tests-class relationship' }
MirrorPrimitivesTest >> testChangingClassOfObject [

	| classObject object |
	classObject := Association.
	object := 0@1.
	
	MirrorPrimitives setClass: classObject to: object.
	
	self assert: object class equals: classObject.
	self assert: object equals: 0->1
]

{ #category : #'tests-fields accessing' }
MirrorPrimitivesTest >> testChangingFixedFieldOfArray [

	self should: [ MirrorPrimitives fixedFieldOf: #(1 2) at: 1 put: 10 ] raise: PrimitiveFailed
]

{ #category : #'tests-fields accessing' }
MirrorPrimitivesTest >> testChangingFixedFieldOfFixedObject [

	| object |
	object := 10@20.
	
	MirrorPrimitives fixedFieldOf: object at: 1 put: 100.	
	self assert: object x equals: 100.
	
	MirrorPrimitives fixedFieldOf: object at: 2 put: 200.	
	self assert: object y equals: 200.
	
	self should: [MirrorPrimitives fixedFieldOf: object at: 3 put: 500] raise: PrimitiveFailed
]

{ #category : #'tests-write barrier' }
MirrorPrimitivesTest >> testChangingFixedFieldOfReadOnlyObject [

	| object |
	object := 10@20.
	object beReadOnlyObject.
	
	self should: [MirrorPrimitives fixedFieldOf: object at: 1 put: 500] raise: ModificationForbidden
]

{ #category : #'tests-fields accessing' }
MirrorPrimitivesTest >> testChangingFixedFieldOfWeakMessageSend [

	| arrayWithInstVars |
	"WeakMessageSend instVarNames #(#selector #shouldBeNil #arguments)"
	arrayWithInstVars := WeakMessageSend receiver: #receiver selector: #selector arguments: #(#args).
	
	MirrorPrimitives fixedFieldOf: arrayWithInstVars at: 1 put: #newSelector.	
	self assert: arrayWithInstVars selector equals: #newSelector.
	
	MirrorPrimitives fixedFieldOf: arrayWithInstVars at: 3 put: #newArgs.	
	self assert: arrayWithInstVars arguments equals: #newArgs.
	
	self
	 	should: [ MirrorPrimitives fixedFieldOf: arrayWithInstVars at: 5 put: 100 ] 
		raise: PrimitiveFailed
]

{ #category : #'tests-fields accessing' }
MirrorPrimitivesTest >> testChangingGeneralFieldOfArray [
	<compilerOptions: #(- #optionReadOnlyLiterals)>
	| object |
	object := #(10 20).
	
	MirrorPrimitives fieldOf: object at: 1 put: 100.	
	self assert: object first equals: 100.
	
	MirrorPrimitives fieldOf: object at: 2 put: 200.	
	self assert: object second equals: 200.
	
	self should: [ MirrorPrimitives fieldOf: object at: 3 put: 500 ] raise: SubscriptOutOfBounds  
]

{ #category : #'tests-fields accessing' }
MirrorPrimitivesTest >> testChangingGeneralFieldOfFixedObject [

	| object |
	object := 10@20.
	
	MirrorPrimitives fieldOf: object at: 1 put: 100.	
	self assert: object x equals: 100.
	
	MirrorPrimitives fieldOf: object at: 2 put: 200.	
	self assert: object y equals: 200.
	
	self should: [ MirrorPrimitives fieldOf: object at: 3 put: 500 ] raise: PrimitiveFailed
]

{ #category : #'tests-fields accessing' }
MirrorPrimitivesTest >> testChangingGeneralFieldOfWeakMessageSend [

	| arrayWithInstVars |
	"WeakMessageSend instVarNames #(#selector #shouldBeNil #arguments)"
	arrayWithInstVars := WeakMessageSend receiver: #receiver selector: #selector arguments: #(#args).
	
	MirrorPrimitives fieldOf: arrayWithInstVars at: 1 put: #newReceiver.	
	self assert: arrayWithInstVars receiver equals: #newReceiver.

	MirrorPrimitives fieldOf: arrayWithInstVars at: 2 put: #newSelector.	
	self assert: arrayWithInstVars selector equals: #newSelector.
	
	MirrorPrimitives fieldOf: arrayWithInstVars at: 4 put: #newArgs.	
	self assert: arrayWithInstVars arguments equals: #newArgs.
	
	self
	 	should: [MirrorPrimitives fieldOf: arrayWithInstVars at: 5 put: 100] 
		raise: PrimitiveFailed
]

{ #category : #'tests-fields accessing' }
MirrorPrimitivesTest >> testChangingIndexableFieldOfArray [
	<compilerOptions: #(- #optionReadOnlyLiterals)>
	| object |
	object := #(10 20).
	
	MirrorPrimitives indexableFieldOf: object at: 1 put: 100.	
	self assert: object first equals: 100.
	
	MirrorPrimitives indexableFieldOf: object at: 2 put: 200.	
	self assert: object second equals: 200.
	
	self should: [MirrorPrimitives indexableFieldOf: object at: 3 put: 500] raise: SubscriptOutOfBounds 
]

{ #category : #'tests-fields accessing' }
MirrorPrimitivesTest >> testChangingIndexableFieldOfFixedObject [
	
	self should: [MirrorPrimitives indexableFieldOf: 10@20 at: 1 put: 10] raise: Error
]

{ #category : #'tests-write barrier' }
MirrorPrimitivesTest >> testChangingIndexableFieldOfReadOnlyArray [
	
	| array |
	array := {10. 20}.
	array beReadOnlyObject.
	
	self should: [ MirrorPrimitives indexableFieldOf: array at: 1 put: 5 ] raise: ModificationForbidden
]

{ #category : #'tests-fields accessing' }
MirrorPrimitivesTest >> testChangingIndexableFieldOfWeakMessageSend [

	| arrayWithInstVars |
	"WeakMessageSend instVarNames #(#selector #shouldBeNil #arguments)"
	arrayWithInstVars := WeakMessageSend receiver: #receiver selector: #selector arguments: #(#args).
	
	MirrorPrimitives indexableFieldOf: arrayWithInstVars at: 1 put: #newReceiver.	
	self assert: arrayWithInstVars receiver equals: #newReceiver.
		
	self
	 	should: [ MirrorPrimitives indexableFieldOf: arrayWithInstVars at: 2 put: 100] 
		raise: SubscriptOutOfBounds 
]

{ #category : #'tests-message performing' }
MirrorPrimitivesTest >> testExecutingMethod [
	| actual |
	
	actual := MirrorPrimitives withReceiver: 0@0 andArguments: {0@2} execute: (Point >> #distanceTo:).
	
	self assert: actual equals: 2
]

{ #category : #'tests-message performing' }
MirrorPrimitivesTest >> testExecutingPrimitive [
	| actual |
	<expectedFailure> "it will be supported by VM at some point"
	
	actual := MirrorPrimitives withReceiver: 100 tryPrimitive: 1 withArguments: #(2).
	
	self assert: actual equals: 102
]

{ #category : #'tests-class relationship' }
MirrorPrimitivesTest >> testGettingClassOfObject [

	| actual |
	actual := MirrorPrimitives classOf: 0@0.
	
	self assert: actual equals: (0@0) class
]

{ #category : #'tests-fields accessing' }
MirrorPrimitivesTest >> testGettingFixedFieldOfArray [
	
	self should: [ MirrorPrimitives fixedFieldOf: #(1 2) at: 1 ] raise: PrimitiveFailed
]

{ #category : #'tests-fields accessing' }
MirrorPrimitivesTest >> testGettingFixedFieldOfFixedObject [

	| actual  |
	
	actual := MirrorPrimitives fixedFieldOf: 10@20 at: 1.	
	self assert: actual equals: 10.
	
	actual := MirrorPrimitives fixedFieldOf: 10@20 at: 2.	
	self assert: actual equals: 20.
	
	self should: [ MirrorPrimitives fixedFieldOf: 10@20 at: 3 ] raise: PrimitiveFailed
]

{ #category : #'tests-fields accessing' }
MirrorPrimitivesTest >> testGettingFixedFieldOfWeakMessageSend [

	| actual  arrayWithInstVars |
	"WeakMessageSend instVarNames #(#selector #shouldBeNil #arguments)"
	arrayWithInstVars := WeakMessageSend receiver: #receiver selector: #selector arguments: #(#args).
	
	actual := MirrorPrimitives fixedFieldOf: arrayWithInstVars at: 1.	
	self assert: actual equals: #selector. "receiver is stored as first array item"
	
	actual := MirrorPrimitives fixedFieldOf: arrayWithInstVars at: 3.	
	self assert: actual first equals: #args.
	
	self should: [ MirrorPrimitives fixedFieldOf: arrayWithInstVars at: 5 ] raise: PrimitiveFailed
]

{ #category : #'tests-fields accessing' }
MirrorPrimitivesTest >> testGettingFixedSizeOfArray [

	| actual  |
	
	actual := MirrorPrimitives fixedSizeOf: #(1 2).
	
	self assert: actual equals: 0
]

{ #category : #'tests-fields accessing' }
MirrorPrimitivesTest >> testGettingFixedSizeOfFixedObject [

	| actual  |
	
	actual := MirrorPrimitives fixedSizeOf: 0@0.
	
	self assert: actual equals: 2
]

{ #category : #'tests-fields accessing' }
MirrorPrimitivesTest >> testGettingFixedSizeOfWeakMessageSend [

	| actual arrayWithInstVars |
	arrayWithInstVars := WeakMessageSend new.
	
	actual := MirrorPrimitives fixedSizeOf: arrayWithInstVars.
	
	self assert: actual equals: WeakMessageSend instSize
]

{ #category : #'tests-fields accessing' }
MirrorPrimitivesTest >> testGettingFullSizeOfArray [

	| actual |
	
	actual := MirrorPrimitives sizeOf: #(1 2).
	
	self assert: actual equals: 2
]

{ #category : #'tests-fields accessing' }
MirrorPrimitivesTest >> testGettingFullSizeOfFixedObject [

	| actual  |
	
	actual := MirrorPrimitives sizeOf: 0@0.
	
	self assert: actual equals: 2
]

{ #category : #'tests-fields accessing' }
MirrorPrimitivesTest >> testGettingFullSizeOfWeakMessageSend [

	| actual arrayWithInstVars |
	arrayWithInstVars := WeakMessageSend new.
	
	actual := MirrorPrimitives sizeOf: arrayWithInstVars.
	
	self assert: actual equals: 1 + WeakMessageSend instSize "receiver of weak message send is stored as first array item"
]

{ #category : #'tests-fields accessing' }
MirrorPrimitivesTest >> testGettingGeneralFieldOfArray [
	
	| actual |
	actual := MirrorPrimitives fieldOf: #(10 20) at: 1.
	self assert: actual equals: 10.
	
	actual := MirrorPrimitives fieldOf: #(110 20) at: 2.
	self assert: actual equals: 20.
	
	self should: [ MirrorPrimitives fieldOf: #(10 20) at: 3 ] raise: SubscriptOutOfBounds  
]

{ #category : #'tests-fields accessing' }
MirrorPrimitivesTest >> testGettingGeneralFieldOfFixedObject [

	| actual  |
	
	actual := MirrorPrimitives fieldOf: 10@20 at: 1.	
	self assert: actual equals: 10.
	
	actual := MirrorPrimitives fieldOf: 10@20 at: 2.	
	self assert: actual equals: 20.
	
	self should: [MirrorPrimitives fieldOf: 10@20 at: 3] raise: PrimitiveFailed
]

{ #category : #'tests-fields accessing' }
MirrorPrimitivesTest >> testGettingGeneralFieldOfWeakMessageSend [

	| actual  arrayWithInstVars |
	"WeakMessageSend instVarNames #(#selector #shouldBeNil #arguments)"
	arrayWithInstVars := WeakMessageSend receiver: #receiver selector: #selector arguments: #(#args).
	
	actual := MirrorPrimitives fieldOf: arrayWithInstVars at: 1.	
	self assert: actual equals: #receiver. "receiver is stored as first array item"
	
	actual := MirrorPrimitives fieldOf: arrayWithInstVars at: 2.	
	self assert: actual equals: #selector.
	
	actual := MirrorPrimitives fieldOf: arrayWithInstVars at: 4.	
	self assert: actual first equals: #args.
	
	self should: [ MirrorPrimitives fieldOf: arrayWithInstVars at: 5 ] raise: PrimitiveFailed
]

{ #category : #'tests-hashes' }
MirrorPrimitivesTest >> testGettingIdentityHashOfObject [

	| actual object |
	object := 0@0.
	
	actual := MirrorPrimitives identityHashOf: object.
	
	self assert: actual equals: object basicIdentityHash
]

{ #category : #'tests-fields accessing' }
MirrorPrimitivesTest >> testGettingIndexableFieldOfArray [
	
	| actual |
	actual := MirrorPrimitives indexableFieldOf: #(10 20) at: 1.
	self assert: actual equals: 10.
	
	actual := MirrorPrimitives indexableFieldOf: #(110 20) at: 2.
	self assert: actual equals: 20.
	
	self should: [MirrorPrimitives indexableFieldOf: #(10 20) at: 3] raise: SubscriptOutOfBounds
]

{ #category : #'tests-fields accessing' }
MirrorPrimitivesTest >> testGettingIndexableFieldOfFixedObject [
	
	self should: [MirrorPrimitives indexableFieldOf: 10@20 at: 1] raise: Error 
]

{ #category : #'tests-fields accessing' }
MirrorPrimitivesTest >> testGettingIndexableFieldOfWeakMessageSend [

	| actual  arrayWithInstVars |
	"WeakMessageSend instVarNames #(#selector #shouldBeNil #arguments)"
	arrayWithInstVars := WeakMessageSend receiver: #receiver selector: #selector arguments: #(#args).
	
	actual := MirrorPrimitives indexableFieldOf: arrayWithInstVars at: 1.	
	self assert: actual equals: #receiver. "receiver is stored as first array item"
	
	self should: [MirrorPrimitives indexableFieldOf: arrayWithInstVars at: 2] raise: SubscriptOutOfBounds
]

{ #category : #'tests-fields accessing' }
MirrorPrimitivesTest >> testGettingIndexibleSizeOfArray [

	| actual |
	
	actual := MirrorPrimitives indexableSizeOf: #(1 2).
	
	self assert: actual equals: 2
]

{ #category : #'tests-fields accessing' }
MirrorPrimitivesTest >> testGettingIndexibleSizeOfFixedObject [

	| actual  |
	
	actual := MirrorPrimitives indexableSizeOf: 0@0.
	
	self assert: actual equals: 0
]

{ #category : #'tests-fields accessing' }
MirrorPrimitivesTest >> testGettingIndexibleSizeOfWeakMessageSend [

	| actual arrayWithInstVars |
	arrayWithInstVars := WeakMessageSend new.
	
	actual := MirrorPrimitives indexableSizeOf: arrayWithInstVars.
	
	self assert: actual equals: 1 "receiver of weak message send is stored as first array item"
]

{ #category : #'tests-comparison' }
MirrorPrimitivesTest >> testIdenticalEquality [

	| object |
	object := 0@0.
	
	self assert: (MirrorPrimitives check: object identicalTo: object).
	self deny: (MirrorPrimitives check: object identicalTo: 0@0)
]

{ #category : #'tests-write barrier' }
MirrorPrimitivesTest >> testMakingObjectReadOnly [
	| point |
	point := 10@20.
	MirrorPrimitives makeObject: point readOnly: true.
	self assert: (MirrorPrimitives isObjectReadOnly: point).
	
	[MirrorPrimitives fieldOf: point at: 1 put: 100.
	self assert: false description: 'should raise modification error'] 
		on: ModificationForbidden do: [].
	self assert: point equals: 10@20.

	MirrorPrimitives makeObject: point readOnly: false.
	self deny: (MirrorPrimitives isObjectReadOnly: point).
	
	MirrorPrimitives fieldOf: point at: 1 put: 100.	
	self assert: point equals: 100@20.
]

{ #category : #'tests-message performing' }
MirrorPrimitivesTest >> testPerformingMessage [
	| actual |
	
	actual := MirrorPrimitives withReceiver: 0@0 perform: #distanceTo: withArguments: {0@2}.
	
	self assert: actual equals: 2
]

{ #category : #'tests-message performing' }
MirrorPrimitivesTest >> testPerformingMessageInSuperclass [
	| actual |
	
	actual := MirrorPrimitives withReceiver: 0@0 perform: #printString withArguments: #() inSuperclass: Object.
	
	self assert: actual equals: '(0@0)'
]

{ #category : #'tests-printing' }
MirrorPrimitivesTest >> testPrintingObject [

	| actual |
	actual := MirrorPrimitives print: 0@0.
	
	self assert: actual equals: 'a Point'
]

{ #category : #'tests-write barrier' }
MirrorPrimitivesTest >> testRetryingFixedFieldModificationOfReadOnlyObject [

	| array actual |
	array := {10. 20}.
	array beReadOnlyObject.
	
	actual := [MirrorPrimitives indexableFieldOf: array at: 1 put: 5. #done] 
		on: ModificationForbidden do: [ :err | 
			array beWritableObject.
			err retryModification ].
	self assert: actual equals: #done.
	self assert: array equals: #(5 20)
]
