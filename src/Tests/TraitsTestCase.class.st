Class {
	#name : #TraitsTestCase,
	#superclass : #TestCase,
	#instVars : [
		'createdClassesAndTraits',
		'oldAnnouncer'
	],
	#category : #'Tests-Traits'
}

{ #category : #testing }
TraitsTestCase class >> isUnitTest [
	^false
]

{ #category : #accessing }
TraitsTestCase class >> resources [
	^ super resources copyWith: TraitsResource
]

{ #category : #utility }
TraitsTestCase >> assertPrints: aString like: anotherString [ 
	self assert: (aString copyWithout: $ )
		= (anotherString copyWithout: $ )
]

{ #category : #accessing }
TraitsTestCase >> c1 [
	^TraitsResource current c1
]

{ #category : #accessing }
TraitsTestCase >> c10 [
	^TraitsResource current c10
]

{ #category : #accessing }
TraitsTestCase >> c11 [
	^TraitsResource current c11
]

{ #category : #accessing }
TraitsTestCase >> c2 [
	^TraitsResource current c2
]

{ #category : #accessing }
TraitsTestCase >> c3 [
	^TraitsResource current c3
]

{ #category : #accessing }
TraitsTestCase >> c4 [
	^TraitsResource current c4
]

{ #category : #accessing }
TraitsTestCase >> c5 [
	^TraitsResource current c5
]

{ #category : #accessing }
TraitsTestCase >> c6 [
	^TraitsResource current c6
]

{ #category : #accessing }
TraitsTestCase >> c7 [
	^TraitsResource current c7
]

{ #category : #accessing }
TraitsTestCase >> c8 [
	^TraitsResource current c8
]

{ #category : #accessing }
TraitsTestCase >> c9 [
	^TraitsResource current c9
]

{ #category : #running }
TraitsTestCase >> categoryName [
	^self class category
]

{ #category : #utility }
TraitsTestCase >> createClassNamed: aSymbol superclass: aClass uses: aTraitComposition [
	| class |
	class := aClass
		subclass: aSymbol
		uses: aTraitComposition
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''		
		category: self categoryName.
	self createdClassesAndTraits add: class.
	^class
]

{ #category : #utility }
TraitsTestCase >> createTraitNamed: aSymbol uses: aTraitComposition [
	| trait |
	trait := Trait
		named: aSymbol
		uses: aTraitComposition
		category: self categoryName.
	self createdClassesAndTraits add: trait.
	^trait
]

{ #category : #utility }
TraitsTestCase >> createdClassesAndTraits [
	createdClassesAndTraits ifNil: [
		createdClassesAndTraits := OrderedCollection new].
	^createdClassesAndTraits
]

{ #category : #utility }
TraitsTestCase >> resourceClassesAndTraits [
	^TraitsResource current createdClassesAndTraits
]

{ #category : #running }
TraitsTestCase >> setUp [

	super setUp.
	oldAnnouncer := SystemAnnouncer uniqueInstance.
	SystemAnnouncer announcer: nil.

]

{ #category : #accessing }
TraitsTestCase >> t1 [
	^TraitsResource current t1
]

{ #category : #accessing }
TraitsTestCase >> t10 [
	^TraitsResource current t10
]

{ #category : #accessing }
TraitsTestCase >> t11 [
	^TraitsResource current t11
]

{ #category : #accessing }
TraitsTestCase >> t12 [
	^TraitsResource current t12
]

{ #category : #accessing }
TraitsTestCase >> t2 [
	^TraitsResource current t2
]

{ #category : #accessing }
TraitsTestCase >> t3 [
	^TraitsResource current t3
]

{ #category : #accessing }
TraitsTestCase >> t4 [
	^TraitsResource current t4
]

{ #category : #accessing }
TraitsTestCase >> t5 [
	^TraitsResource current t5
]

{ #category : #accessing }
TraitsTestCase >> t6 [
	^TraitsResource current t6
]

{ #category : #accessing }
TraitsTestCase >> t8 [
	^TraitsResource current t8
]

{ #category : #accessing }
TraitsTestCase >> t9 [
	^TraitsResource current t9
]

{ #category : #running }
TraitsTestCase >> tearDown [
	TraitsResource reset.
	self createdClassesAndTraits
		do: [ :aClassOrTrait || behaviorName |
			behaviorName := aClassOrTrait name.
			Smalltalk globals at: behaviorName ifPresent: [ :classOrTrait | classOrTrait removeFromSystem ].
			ChangeSet current removeClassChanges: behaviorName ].
	createdClassesAndTraits := nil.
	SystemAnnouncer announcer: oldAnnouncer.
	super tearDown 

]
