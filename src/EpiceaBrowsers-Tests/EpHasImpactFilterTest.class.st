Class {
	#name : 'EpHasImpactFilterTest',
	#superclass : 'TestCase',
	#instVars : [
		'classFactory',
		'impactFilter'
	],
	#category : 'EpiceaBrowsers-Tests-Filters',
	#package : 'EpiceaBrowsers-Tests',
	#tag : 'Filters'
}

{ #category : 'asserting' }
EpHasImpactFilterTest class >> shouldLogWithEpicea [

	^ true
]

{ #category : 'running' }
EpHasImpactFilterTest >> setUp [
	super setUp.
	classFactory := ClassFactoryForTestCase new.
	impactFilter := EpImpactCodeChangeFilter new
		environment: self class environment;
		yourself
]

{ #category : 'running' }
EpHasImpactFilterTest >> tearDown [
	classFactory cleanUp.
	super tearDown
]

{ #category : 'tests' }
EpHasImpactFilterTest >> testClassSideMethodWithImpact [
	| anEvent aClass |
	aClass := classFactory newClass classSide.
	aClass compile: 'a ^1'.
	anEvent := OmEntry content: (EpMethodAddition method: aClass >> #a).
	aClass compile: 'a ^2'.
	self assert: (impactFilter accepts: anEvent)
]

{ #category : 'tests' }
EpHasImpactFilterTest >> testClassSideMethodWithoutImpact [
	| anEvent aClass |
	aClass := classFactory newClass classSide.
	aClass compile: 'a ^1'.
	anEvent := OmEntry content: (EpMethodAddition method: aClass >> #a).
	self deny: (impactFilter accepts: anEvent)
]

{ #category : 'tests' }
EpHasImpactFilterTest >> testClassWithClassSideImpact [
	| anEvent aClass |
	aClass := classFactory newClass.
	anEvent := OmEntry content: (EpClassAddition class: aClass).
	aClass classSide addSlot: #a.
	self assert: (impactFilter accepts: anEvent)
]

{ #category : 'tests' }
EpHasImpactFilterTest >> testClassWithInstanceSideImpact [
	| anEvent aClass |
	aClass := classFactory newClass.
	anEvent := OmEntry content: (EpClassAddition class: aClass).
	aClass addSlot: #a.
	self assert: (impactFilter accepts: anEvent)
]

{ #category : 'tests' }
EpHasImpactFilterTest >> testClassWithoutImpact [
	| anEvent aClass |
	aClass := classFactory newClass.
	anEvent := OmEntry content: (EpClassAddition class: aClass).
	self deny: (impactFilter accepts: anEvent)
]

{ #category : 'tests' }
EpHasImpactFilterTest >> testCommentWithImpact [
	| anEvent aClass |
	aClass := classFactory newClass.
	aClass comment: 'a'.
	aClass comment: 'b'.
	anEvent := OmEntry content: (EpBehaviorCommentChange
		newWithBehavior: aClass asEpiceaRingDefinition
		oldComment: 'a'
		newComment: 'b'
		oldStamp: aClass commentStamp
		newStamp: aClass commentStamp).
	aClass comment: 'c'.
	self assert: (impactFilter accepts: anEvent)
]

{ #category : 'tests' }
EpHasImpactFilterTest >> testCommentWithoutImpact [
	| anEvent aClass |
	aClass := classFactory newClass.
	aClass comment: 'a'.
	anEvent := OmEntry content: (EpBehaviorCommentChange
		newWithBehavior: aClass asEpiceaRingDefinition
		oldComment: 'a'
		newComment: 'b'
		oldStamp: aClass commentStamp
		newStamp: aClass commentStamp).
	aClass comment: 'c'.
	aClass comment: 'b'.
	self deny: (impactFilter accepts: anEvent)
]

{ #category : 'tests' }
EpHasImpactFilterTest >> testMethodWithImpact [
	| anEvent aClass |
	aClass := classFactory newClass.
	aClass compile: 'a ^1'.
	anEvent := OmEntry content: (EpMethodAddition method: aClass >> #a).
	aClass compile: 'a ^2'.
	self assert: (impactFilter accepts: anEvent)
]

{ #category : 'tests' }
EpHasImpactFilterTest >> testMethodWithoutImpact [
	| anEvent aClass |
	aClass := classFactory newClass.
	aClass compile: 'a ^1'.
	anEvent := OmEntry content: (EpMethodAddition method: aClass >> #a).
	self deny: (impactFilter accepts: anEvent)
]

{ #category : 'tests' }
EpHasImpactFilterTest >> testProtocolRemovalOfTraitProtocol [

	| anEntry aClass |
	aClass := classFactory make: [ :aBuilder | aBuilder traitComposition: TAssertable ].
	anEntry := OmEntry content: (EpProtocolRemoval behavior: aClass asRingDefinition protocol: #private).
	self deny: (impactFilter accepts: anEntry).

	aClass compile: 'a ^1' classified: #private.

	self assert: (impactFilter accepts: anEntry)
]
