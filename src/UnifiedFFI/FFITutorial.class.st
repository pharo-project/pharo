Class {
	#name : #FFITutorial,
	#superclass : #Object,
	#classVars : [
		'Age'
	],
	#category : #'UnifiedFFI-Types'
}

{ #category : #'class initialization' }
FFITutorial class >> initialize [

	Age := #uint
]

{ #category : #'as yet unclassified' }
FFITutorial >> abs: n [
	^ self ffiCall: #( Age abs ( Age n ) ) module: LibC
]
