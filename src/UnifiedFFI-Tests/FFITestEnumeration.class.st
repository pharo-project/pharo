"
An enumeration for test purposes
"
Class {
	#name : #FFITestEnumeration,
	#superclass : #FFIExternalEnumeration,
	#classVars : [
		'AAA',
		'BBB',
		'CCC',
		'DDD'
	],
	#category : #'UnifiedFFI-Tests'
}

{ #category : #'accessing enum' }
FFITestEnumeration class >> AAA [
	"This method was automatically generated"
	^ AAA
]

{ #category : #'accessing enum' }
FFITestEnumeration class >> BBB [
	"This method was automatically generated"
	^ BBB
]

{ #category : #'accessing enum' }
FFITestEnumeration class >> CCC [
	"This method was automatically generated"
	^ CCC
]

{ #category : #'accessing enum' }
FFITestEnumeration class >> DDD [
	"This method was automatically generated"
	^ DDD
]

{ #category : #'enum declaration' }
FFITestEnumeration class >> enumDecl [
	^ #(AAA 1
	BBB  2
	CCC 3
	DDD 2400) 
]

{ #category : #'class initialization' }
FFITestEnumeration class >> initialize [
	self initializeEnumeration
]
