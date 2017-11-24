"
See comment of my superclass TraitTransformation.
"
Class {
	#name : #TraitExclusion,
	#superclass : #TraitTransformation,
	#instVars : [
		'exclusions'
	],
	#category : #'Traits-Composition'
}

{ #category : #'instance creation' }
TraitExclusion class >> with: aTraitComposition exclusions: anArrayOfSelectors [
	^self new
		subject: aTraitComposition;
		exclusions: anArrayOfSelectors;
		yourself

]

{ #category : #composition }
TraitExclusion >> - anArrayOfSelectors [
	^TraitExclusion
		with: subject
		exclusions: (anArrayOfSelectors, exclusions asArray)
]

{ #category : #composition }
TraitExclusion >> addExclusionOf: aSymbol [
	self exclusions: (self exclusions copyWith: aSymbol)
]

{ #category : #enquiries }
TraitExclusion >> allSelectors [
	| selectors |
	selectors := self subject allSelectors.
	self exclusions do: [:each |
		selectors remove: each ifAbsent: []].
	^selectors
]

{ #category : #enquiries }
TraitExclusion >> collectMethodsFor: aSelector into: methodDescription [
	(self exclusions includes: aSelector) ifFalse: [
		self subject
			collectMethodsFor: aSelector
			into: methodDescription]
]

{ #category : #copying }
TraitExclusion >> copyTraitExpression [
	^super copyTraitExpression 
		exclusions: self exclusions deepCopy;
		yourself
]

{ #category : #accessing }
TraitExclusion >> exclusions [
	^exclusions
]

{ #category : #accessing }
TraitExclusion >> exclusions: aCollection [
	exclusions := aCollection
]

{ #category : #testing }
TraitExclusion >> isEmpty [
	^self exclusions isEmpty
]

{ #category : #copying }
TraitExclusion >> postCopy [
	super postCopy.
	exclusions := exclusions copy
]

{ #category : #printing }
TraitExclusion >> printOn: aStream [
	super printOn: aStream. 
	aStream
		space;
		nextPut: $-;
		space;
		nextPut: ${.
	self exclusions do: [:each | aStream print: each]
		separatedBy: [aStream nextPutAll: '. '].
	aStream nextPut: $}
]

{ #category : #comparing }
TraitExclusion >> syntacticallyEquals: aTraitTransformation [
	"Test syntactic equivalence of this trait transformation with another. 
	The order of the exclusions is taken into account. For example, A - { #x. #y } ~= A - { #y. #x }."

	self == aTraitTransformation ifTrue: [ ^ true ].
	self species = aTraitTransformation species ifFalse: [ ^ false ].
	^ self exclusions = aTraitTransformation exclusions
]
