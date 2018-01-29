"
I'm a context for make tests run. 
I'm polymorphic with Context and in some way also with CompiledMethod (but just in the context of the execution of the tests for method builder).
"
Class {
	#name : #FFICalloutMethodBuilderTestContext,
	#superclass : #Object,
	#instVars : [
		'selector',
		'argumentNames',
		'methodClass'
	],
	#category : #'UnifiedFFI-Tests'
}

{ #category : #accessing }
FFICalloutMethodBuilderTestContext >> argumentNames [
	^ argumentNames
]

{ #category : #accessing }
FFICalloutMethodBuilderTestContext >> argumentNames: anObject [
	argumentNames := anObject
]

{ #category : #accessing }
FFICalloutMethodBuilderTestContext >> method [
	^ self
]

{ #category : #accessing }
FFICalloutMethodBuilderTestContext >> methodClass [
	^ methodClass
]

{ #category : #accessing }
FFICalloutMethodBuilderTestContext >> methodClass: anObject [
	methodClass := anObject
]

{ #category : #accesing }
FFICalloutMethodBuilderTestContext >> methodProperties [
	"stub"

	^ nil
]

{ #category : #accessing }
FFICalloutMethodBuilderTestContext >> selector [
	^ selector
]

{ #category : #accessing }
FFICalloutMethodBuilderTestContext >> selector: anObject [
	selector := anObject
]

{ #category : #accessing }
FFICalloutMethodBuilderTestContext >> trailer [
	^ CompiledMethodTrailer empty
]
