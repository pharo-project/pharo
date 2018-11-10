"
I'm a base class to provide different type of MCRepositories from iceberg, so metacello can do his work of installing packages.
My children define specific types, visit them to have more details of what you can do.
"
Class {
	#name : #IceMetacelloRepositoryType,
	#superclass : #Object,
	#instVars : [
		'location'
	],
	#category : #'Iceberg-Metacello-Integration'
}

{ #category : #accessing }
IceMetacelloRepositoryType class >> allTypes [
	^ self allSubclasses select: [ :each | each isAbstract not ]
]

{ #category : #testing }
IceMetacelloRepositoryType class >> canHandleType: aType [ 
	^ self allTypes
		anySatisfy: [ :each | each isSuitableForType: aType ]  
	
]

{ #category : #private }
IceMetacelloRepositoryType class >> extractTypeOf: aString [ 
	^ aString copyUpTo: $:
]

{ #category : #'instance creation' }
IceMetacelloRepositoryType class >> for: aLocationString [
	^ (self allTypes
		detect: [ :each | each isSuitableForLocation: aLocationString ])
		location: aLocationString
]

{ #category : #testing }
IceMetacelloRepositoryType class >> isAbstract [
	^ self = IceMetacelloRepositoryType
]

{ #category : #testing }
IceMetacelloRepositoryType class >> isSuitableForLocation: aLocationString [
	^ self isSuitableForType: (self extractTypeOf: aLocationString) 
]

{ #category : #testing }
IceMetacelloRepositoryType class >> isSuitableForType: aType [
	^ self type = aType
]

{ #category : #private }
IceMetacelloRepositoryType class >> location: aLocationString [
	self = IceMetacelloRepositoryType 
		ifTrue: [ self error: 'I''m an abstract class, use one of my children instead.' ].
	
	^ self basicNew
		initializeLocation: aLocationString;
		yourself
]

{ #category : #accessing }
IceMetacelloRepositoryType class >> type [
	^ self subclassResponsibility
]

{ #category : #initialization }
IceMetacelloRepositoryType >> initializeLocation: aString [
	location := aString.
	self initialize
]

{ #category : #accessing }
IceMetacelloRepositoryType >> location [
	^ location
]

{ #category : #accessing }
IceMetacelloRepositoryType >> mcRepository [
	self subclassResponsibility
]
