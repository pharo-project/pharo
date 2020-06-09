Class {
	#name : #Variable,
	#superclass : #Object,
	#instVars : [
		'name'
	],
	#classVars : [
		'Properties'
	],
	#category : #'Slot-Core-Variables'
}

{ #category : #'class initialization' }
Variable class >> initialize [
	Properties := Properties
		ifNil: [ WeakIdentityKeyDictionary new. ]
		ifNotNil: [ (WeakIdentityKeyDictionary newFrom: Properties) rehash]
]

{ #category : #comparing }
Variable >> = other [
	"Every subclass that adds state must redefine this method"
	self == other
		ifTrue: [ ^ true ].
	^ (self species == other species) 
			and: [ name = other name ]
]

{ #category : #properties }
Variable >> ensureProperties [
	^ Properties at: self ifAbsentPut: WeakKeyDictionary new
]

{ #category : #properties }
Variable >> hasProperty: aKey [
	"Test if the property aKey is present."
	^self properties 
		ifNil: [false]
		ifNotNil: [:prop | prop includesKey: aKey]
]

{ #category : #comparing }
Variable >> hash [
	"Every subclass that adds state must redefine this method"
	^ self species hash bitXor: self name hash
]

{ #category : #accessing }
Variable >> name [
	^ name
]

{ #category : #accessing }
Variable >> name: aSymbol [
	name := aSymbol
]

{ #category : #properties }
Variable >> properties [
	^ Properties at: self ifAbsent: nil
		
]

{ #category : #properties }
Variable >> propertyAt: propName [
	^ self
		propertyAt: propName
		ifAbsent: [ nil ]
		
]

{ #category : #properties }
Variable >> propertyAt: propName ifAbsent: aBlock [
	self properties ifNil: [^aBlock value].
	^ self properties
		at: propName
		ifAbsent: aBlock
		
]

{ #category : #properties }
Variable >> propertyAt: aKey ifAbsentPut: aBlock [
	"Answer the property associated with aKey or, if aKey isn't found store the result of evaluating aBlock as new value."
	
	^ self propertyAt: aKey ifAbsent: [ self propertyAt: aKey put: aBlock value ]
]

{ #category : #properties }
Variable >> propertyAt: propName put: propValue [
	^ self ensureProperties
		at: propName
		put: propValue
]

{ #category : #properties }
Variable >> removePropertiesIfEmpty [
	^ Properties at: self ifPresent: [ :dict |
		dict ifEmpty: [ Properties removeKey: self ] ]
]

{ #category : #properties }
Variable >> removeProperty: propName [
	^ self
		removeProperty: propName
		ifAbsent: [ nil ]
]

{ #category : #properties }
Variable >> removeProperty: propName ifAbsent: aBlock [
	| property |
	self properties ifNil: [^aBlock value].
	property := self properties
		removeKey: propName
		ifAbsent: aBlock.
	self removePropertiesIfEmpty.
	^ property
]
