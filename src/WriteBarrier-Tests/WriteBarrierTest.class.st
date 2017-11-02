"
My tests ensure the ReadOnly property of objects work properly.

#testMutateIVObject is a good start to understand what is going on.

The VM needs to be compiled with -DIMMUTABILTY= true for those tests to work.
"
Class {
	#name : #WriteBarrierTest,
	#superclass : #TestCase,
	#classVars : [
		'ContextInstance'
	],
	#category : 'WriteBarrier-Tests'
}

{ #category : #initialization }
WriteBarrierTest class >> initialize [
	
	ContextInstance := Context sender: nil receiver: self new method: self >> #alwaysWritableObjects arguments: #()
]

{ #category : #'guinea pigs' }
WriteBarrierTest >> alwaysReadOnlyObjects [
	"Immediates are always immutable"
	^ { 1 }
]

{ #category : #'guinea pigs' }
WriteBarrierTest >> alwaysWritableObjects [
	"Objects that currently can't be immutable"
	^ { ContextInstance . 
		Processor . 
		Processor activeProcess }
]

{ #category : #'guinea pigs' }
WriteBarrierTest >> maybeReadOnlyObjects [
	"ByteObject, Variable object, fixed sized object"
	^ { { 1 . 2 . 3 } asByteArray . { 1 . 2 . 3 } . (MessageSend receiver: 1 selector: #+ argument: 2) }
]

{ #category : #'tests - proxy' }
WriteBarrierTest >> testBasicProxyReadOnly [
	self alwaysReadOnlyObjects do: [ :each |
		self assert: (MirrorPrimitives isObjectReadOnly: each) equals: true ]
]

{ #category : #'tests - proxy' }
WriteBarrierTest >> testBasicProxyWritable [
	self alwaysWritableObjects , self maybeReadOnlyObjects do: [ :each |
		self assert: (MirrorPrimitives isObjectReadOnly: each) equals: false ]
]

{ #category : #'tests - object' }
WriteBarrierTest >> testBasicReadOnly [
	self alwaysReadOnlyObjects do: [ :each |
		self assert: each isReadOnlyObject equals: true ]
]

{ #category : #'tests - object' }
WriteBarrierTest >> testBasicWritable [
	self alwaysWritableObjects , self maybeReadOnlyObjects do: [ :each |
		self assert: each isReadOnlyObject equals: false ]
]

{ #category : #'tests - object' }
WriteBarrierTest >> testMutateFirstInstVarOfObjectWithManyVars [
	| guineaPig failure |
	guineaPig := WriteBarrierStub new.
	guineaPig beReadOnlyObject.
	failure := [ guineaPig var1: #test ] on: ModificationForbidden do: [:err | err].

	self assert: failure fieldIndex equals: 1
]

{ #category : #'tests - object' }
WriteBarrierTest >> testMutateIVObject [
	| guineaPig |
	guineaPig := MessageSend new.
	guineaPig beReadOnlyObject.
	[ guineaPig receiver: 1 ] 
		on: ModificationForbidden 
		do: [ "Surely a NoModification error" ].
	guineaPig
		beWritableObject;
		selector: #+;
		beReadOnlyObject.
	[ guineaPig arguments: #(2) ] 
		on: ModificationForbidden 
		do: [ "Surely a NoModification error" ].
	self assert: guineaPig receiver isNil.
	self assert: guineaPig arguments isNil.
	self assert: guineaPig selector == #+.
]

{ #category : #'tests - object' }
WriteBarrierTest >> testMutateInstVarShouldCatchRightFailure [
	| guineaPig failure |
	guineaPig := MessageSend new.
	guineaPig beReadOnlyObject.
	failure := [ guineaPig receiver: #test ] on: ModificationForbidden do: [:err | err].

	self assert: failure object == guineaPig.
	self assert: failure newValue equals: #test.
	self assert: failure fieldIndex equals: 1.
]

{ #category : #'tests - object' }
WriteBarrierTest >> testMutateLastInstVarOfObjectWithManyVars [
	| guineaPig failure |
	guineaPig := WriteBarrierStub new.
	guineaPig beReadOnlyObject.
	failure := [ guineaPig var10: #test ] on: ModificationForbidden do: [:err | err].

	self assert: failure fieldIndex equals: 10
]

{ #category : #'tests - object' }
WriteBarrierTest >> testMutateVariableObject [
	| guineaPigs |
	guineaPigs := {#[1 2 3] . #(1 2 3) }.
	guineaPigs do: [ :guineaPig | 
		guineaPig beReadOnlyObject.
		[guineaPig at: 1 put: 4] 
			on: Error 
			do: [ "Surely a NoModification error" ].
		guineaPig
			beWritableObject;
			at: 2 put:  5;
			beReadOnlyObject.
		[guineaPig at: 3 put: 6] 
			on: Error 
			do: [ "Surely a NoModification error" ].
		self assert: guineaPig first = 1.
		self assert: guineaPig second = 5.
		self assert: guineaPig third = 3 ]
]

{ #category : #'tests - helper' }
WriteBarrierTest >> testObject: object initialState: initialState tuples: tuples [
	self 
		testObject: object 
		initialState: initialState 
		tuples: tuples 
		setReadOnlyBlock: [ :value | object setIsReadOnlyObject: value ]
]

{ #category : #'tests - helper' }
WriteBarrierTest >> testObject: object initialState: initialState tuples: tuples setReadOnlyBlock: setImmutabilityBlock [
	self assert: object isReadOnlyObject equals: initialState.
	tuples do: [ :tuple |
		| stateToSet expectedResult expectedNewState |
		stateToSet := tuple first.
		expectedResult := tuple second.
		expectedNewState := tuple last.
		[self assert: (setImmutabilityBlock value: stateToSet) equals: expectedResult ]
				on: PrimitiveFailed 
				do: [ self assert: (self alwaysReadOnlyObjects , self alwaysWritableObjects includes: object) ].
		self assert: object isReadOnlyObject equals: expectedNewState ]
]

{ #category : #'tests - helper' }
WriteBarrierTest >> testProxyObject: object initialState: initialState tuples: tuples [
	self 
		testObject: object 
		initialState: initialState 
		tuples: tuples 
		setReadOnlyBlock: [ :value | 
			MirrorPrimitives makeObject: object readOnly: value ]
]

{ #category : #'tests - object' }
WriteBarrierTest >> testRetryingInstVarModification [
	| guineaPig |
	guineaPig := MessageSend new.
	guineaPig beReadOnlyObject.

	[ guineaPig receiver: 1 ] on: ModificationForbidden do: [:err | 
		guineaPig beWritableObject.
		err retryModification ].

	self assert: guineaPig receiver equals: 1
]

{ #category : #'tests - object' }
WriteBarrierTest >> testSetIsReadOnlyFailure [
	self alwaysWritableObjects do: [ :each |
		self 
			testObject: each 
			initialState: false 
			tuples: #( (true false false) (false false false) ) ]
	
]

{ #category : #'tests - proxy' }
WriteBarrierTest >> testSetIsReadOnlyFailureProxy [
	self alwaysWritableObjects do: [ :each |
		self 
			testProxyObject: each 
			initialState: false 
			tuples: #( (true false false) (false false false) ) ]
	
]

{ #category : #'tests - object' }
WriteBarrierTest >> testSetIsReadOnlyImmediate [
	self alwaysReadOnlyObjects do: [ :each |
		self 
			testObject: each 
			initialState: true 
			tuples: #( (true true true) (false true true) ) ]
	
]

{ #category : #'tests - proxy' }
WriteBarrierTest >> testSetIsReadOnlyImmediateProxy [
	self alwaysReadOnlyObjects do: [ :each |
		self 
			testProxyObject: each 
			initialState: true 
			tuples: #( (true true true) (false true true) ) ]
	
]

{ #category : #'tests - object' }
WriteBarrierTest >> testSetIsReadOnlySuccess [
	self maybeReadOnlyObjects do: [ :each |
		self 
			testObject: each 
			initialState: false 
			tuples: #( (true false true) (false true false) ) ]
	
]

{ #category : #'tests - proxy' }
WriteBarrierTest >> testSetIsReadOnlySuccessProxy [
	self maybeReadOnlyObjects do: [ :each |
		self 
			testProxyObject: each 
			initialState: false 
			tuples: #( (true false true) (false true false) ) ]
	
]
