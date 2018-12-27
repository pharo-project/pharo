"
I am an abstract class

I am used to link a spec-oriented adapter name to a framework specific adapter class name
"
Class {
	#name : #SpecAdapterBindings,
	#superclass : #Object,
	#instVars : [
		'bindings'
	],
	#category : #'Spec-Core-Base'
}

{ #category : #testing }
SpecAdapterBindings class >> isAbstract [ 

	^ self name = #SpecAdapterBindings
]

{ #category : #initialize }
SpecAdapterBindings >> abstractAdapterClass [
	
	^ self subclassResponsibility
]

{ #category : #'as yet unclassified' }
SpecAdapterBindings >> adapterClass: aSymbol [ 
	
	^ Smalltalk 
		at: (self translateSymbol: aSymbol) 
		ifAbsent: [ aSymbol ]
]

{ #category : #initialize }
SpecAdapterBindings >> allAdapters [
	^ self abstractAdapterClass allAdapters
]

{ #category : #initialize }
SpecAdapterBindings >> initialize [

	super initialize.

	bindings := Dictionary new.
	self initializeBindings
]

{ #category : #initialize }
SpecAdapterBindings >> initializeBindings [
	
	self allAdapters do: [ :each |
		bindings 
			at: each adaptingName 
			put: each name.
		each adaptingAliases do: [ :eachAlias |
			bindings 
				at: eachAlias 
				put: each name ] ]	
]

{ #category : #protocol }
SpecAdapterBindings >> translateSymbol: aSymbol [

	^ bindings 
		at: aSymbol
		ifAbsent: [ aSymbol ]
]
