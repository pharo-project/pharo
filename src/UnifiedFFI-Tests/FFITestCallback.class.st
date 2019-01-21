Class {
	#name : #FFITestCallback,
	#superclass : #FFICallback,
	#classVars : [
		'ARRAY'
	],
	#category : #'UnifiedFFI-Tests'
}

{ #category : #'as yet unclassified' }
FFITestCallback class >> arrayType: aType [

	ARRAY := aType
]
