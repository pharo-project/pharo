"
I am an example of how dynamically create the UI corresponding at an object.

self exampleInt
self exampleNil
self exampleString
"
Class {
	#name : #SpDynamicSpecExample,
	#superclass : #Object,
	#traits : 'SpTObservable',
	#classTraits : 'SpTObservable classTrait',
	#instVars : [
		'#ui',
		'#object => SpObservableSlot'
	],
	#category : #'Spec2-Examples-Wrapper'
}

{ #category : #example }
SpDynamicSpecExample class >> exampleInt [
	<sampleInstance>
	^ self new open: 42
]

{ #category : #example }
SpDynamicSpecExample class >> exampleNil [
	<sampleInstance>
	^ self new
		open: nil
]

{ #category : #example }
SpDynamicSpecExample class >> exampleString [
	<sampleInstance>
	^ self new
		open: 'Hello World'
]

{ #category : #'instance creation' }
SpDynamicSpecExample class >> object: object [

	^ self new
		object: object;
		yourself
]

{ #category : #initialization }
SpDynamicSpecExample >> initialize [
	self class initializeSlots: self.
	super initialize
]

{ #category : #accessing }
SpDynamicSpecExample >> object [
	^ object
]

{ #category : #accessing }
SpDynamicSpecExample >> object: anObject [
	object := anObject
]

{ #category : #protocol }
SpDynamicSpecExample >> open [
	self object ifNil: [ ^ self openOnNil ].
	self object isInteger ifTrue: [ ^ self openOnInteger: self object ].
	self object isString ifTrue: [ ^ self openOnString: self object ]
]

{ #category : #protocol }
SpDynamicSpecExample >> open: anObject [
	object := anObject.
	self open
]

{ #category : #protocol }
SpDynamicSpecExample >> openOnInteger: anInteger [
	ui := SpOpenOnIntExample new: anInteger.
	ui
		extent: 100 @ 100;
		openWithSpec
]

{ #category : #protocol }
SpDynamicSpecExample >> openOnNil [
	ui := SpOpenOnNilExample new.
	ui 
		extent: 200 @ 100; openWithSpec
]

{ #category : #protocol }
SpDynamicSpecExample >> openOnString: aString [
	ui := SpOpenOnStringExample new:aString.
	ui 
		extent: 300 @ 200;openWithSpec
]

{ #category : #accessing }
SpDynamicSpecExample >> title [

	^ 'Dynamic Examples'
]
