"
Tests for FFIConstantHandle
"
Class {
	#name : #FFIConstantHandleTest,
	#superclass : #TestCase,
	#classVars : [
		'TIME_T_PTR'
	],
	#category : #'UnifiedFFI-Tests-Tests'
}

{ #category : #primitives }
FFIConstantHandleTest >> getTime: t [
     ^self ffiCall: #(long time(FFIConstantHandle *t) ) module: LibC
]

{ #category : #primitives }
FFIConstantHandleTest >> getTimeReturnConstantHandle: t [
     ^self ffiCall: #(FFIConstantHandle time(TIME_T_PTR t)) module: LibC
]

{ #category : #tests }
FFIConstantHandleTest >> testCall [
	| object time |
	
	object := FFIConstantHandle new.
	time := self getTime: object.
	self assert: time equals: object handle
]

{ #category : #tests }
FFIConstantHandleTest >> testReturn [
	| object time |
	
	TIME_T_PTR := FFIExternalValueHolder ofType: 'long'.
	
	object := TIME_T_PTR new value: 0.
	time := self getTimeReturnConstantHandle: object.
	self assert: time handle equals: object value
]
