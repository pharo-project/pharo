"
I am a test class for running DebugPointTest tests.
"
Class {
	#name : 'DummyTestClass',
	#superclass : 'Object',
	#instVars : [
		'id'
	],
	#classVars : [
		'ClassVar'
	],
	#category : 'DebugPoints-Tests-Utils',
	#package : 'DebugPoints-Tests',
	#tag : 'Utils'
}

{ #category : 'test methods' }
DummyTestClass class >> dummyClassSide [

	^ 42
]

{ #category : 'nil' }
DummyTestClass >> classVarRead [

	^ ClassVar
]

{ #category : 'accessing' }
DummyTestClass >> classVarWrite [
	^ClassVar := 1
]

{ #category : 'accessing' }
DummyTestClass >> id [

	^ id
]

{ #category : 'accessing' }
DummyTestClass >> id: anId [
	^id := anId
]

{ #category : 'initialization' }
DummyTestClass >> initialize [

	super initialize.
	id := 0
]
