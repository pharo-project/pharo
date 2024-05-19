"
I represent an not fully visited selector in a core result.

For a given `selector`, I associate callers that are call sites (`RBSendNode`) in covered methods to callees that are uncovered methods (`CompiledMethod`).
"
Class {
	#name : 'Crown',
	#superclass : 'Object',
	#instVars : [
		'selector',
		'callees',
		'callers'
	],
	#category : 'Coverage-Base',
	#package : 'Coverage',
	#tag : 'Base'
}

{ #category : 'accessing' }
Crown >> callees [

	^ callees
]

{ #category : 'accessing' }
Crown >> callers [

	^ callers
]

{ #category : 'initialization' }
Crown >> initialize [

	super initialize.
	callees := OrderedCollection new.
	callers := OrderedCollection new
]

{ #category : 'printing' }
Crown >> printOn: aStream [

	super printOn: aStream.
	aStream print: selector.
	aStream
		nextPutAll: ' callers: ';
		print: callers size.
	aStream
		nextPutAll: ' callees: ';
		print: callees size.

]

{ #category : 'accessing' }
Crown >> selector [

	^ selector
]

{ #category : 'accessing' }
Crown >> selector: anObject [

	selector := anObject
]
