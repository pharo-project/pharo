"
I am an abstract class

I am used to link a spec-oriented adapter name to a framework specific adapter class name
"
Class {
	#name : #SpAdapterBindings,
	#superclass : #Object,
	#instVars : [
		'bindings'
	],
	#category : #'Spec2-Core-Base'
}

{ #category : #testing }
SpAdapterBindings class >> isAbstract [ 

	^ self name = #SpAdapterBindings
]

{ #category : #initialize }
SpAdapterBindings >> abstractAdapterClass [
	
	^ self subclassResponsibility
]

{ #category : #'as yet unclassified' }
SpAdapterBindings >> adapterClass: aSymbol [ 
	
	^ Smalltalk 
		at: (self translateSymbol: aSymbol) 
		ifAbsent: [ aSymbol ]
]

{ #category : #initialize }
SpAdapterBindings >> allAdapters [
	^ self abstractAdapterClass allAdapters
]

{ #category : #initialization }
SpAdapterBindings >> initialize [

	super initialize.

	bindings := Dictionary new.
	self initializeBindings
]

{ #category : #initialize }
SpAdapterBindings >> initializeBindings [
	
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
SpAdapterBindings >> translateSymbol: aSymbol [

	^ bindings 
		at: aSymbol
		ifAbsent: [ aSymbol ]
]
