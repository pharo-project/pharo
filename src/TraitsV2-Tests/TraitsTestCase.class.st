Class {
	#name : 'TraitsTestCase',
	#superclass : 'T2AbstractTest',
	#category : 'TraitsV2-Tests',
	#package : 'TraitsV2-Tests'
}

{ #category : 'testing' }
TraitsTestCase class >> isUnitTest [
	^false
]

{ #category : 'accessing' }
TraitsTestCase class >> resources [
	^ super resources copyWith: TraitsResource
]

{ #category : 'utilities' }
TraitsTestCase >> assertPrints: aString like: anotherString [
	self assert: (aString copyWithout: $ ) equals: (anotherString copyWithout: $ )
]

{ #category : 'accessing' }
TraitsTestCase >> c1 [
	^TraitsResource current c1
]

{ #category : 'accessing' }
TraitsTestCase >> c10 [
	^TraitsResource current c10
]

{ #category : 'accessing' }
TraitsTestCase >> c11 [
	^TraitsResource current c11
]

{ #category : 'accessing' }
TraitsTestCase >> c2 [
	^TraitsResource current c2
]

{ #category : 'accessing' }
TraitsTestCase >> c3 [
	^TraitsResource current c3
]

{ #category : 'accessing' }
TraitsTestCase >> c4 [
	^TraitsResource current c4
]

{ #category : 'accessing' }
TraitsTestCase >> c5 [
	^TraitsResource current c5
]

{ #category : 'accessing' }
TraitsTestCase >> c6 [
	^TraitsResource current c6
]

{ #category : 'accessing' }
TraitsTestCase >> c7 [
	^TraitsResource current c7
]

{ #category : 'accessing' }
TraitsTestCase >> c8 [
	^TraitsResource current c8
]

{ #category : 'accessing' }
TraitsTestCase >> c9 [
	^TraitsResource current c9
]

{ #category : 'utilities' }
TraitsTestCase >> createdClassesAndTraits [

	^ (self packageOrganizer packageNamed: self packageNameForTests) definedClasses
]

{ #category : 'accessing' }
TraitsTestCase >> t1 [
	^TraitsResource current t1
]

{ #category : 'accessing' }
TraitsTestCase >> t10 [
	^TraitsResource current t10
]

{ #category : 'accessing' }
TraitsTestCase >> t11 [
	^TraitsResource current t11
]

{ #category : 'accessing' }
TraitsTestCase >> t12 [
	^TraitsResource current t12
]

{ #category : 'accessing' }
TraitsTestCase >> t2 [
	^TraitsResource current t2
]

{ #category : 'accessing' }
TraitsTestCase >> t3 [
	^TraitsResource current t3
]

{ #category : 'accessing' }
TraitsTestCase >> t4 [
	^TraitsResource current t4
]

{ #category : 'accessing' }
TraitsTestCase >> t5 [
	^TraitsResource current t5
]

{ #category : 'accessing' }
TraitsTestCase >> t6 [
	^TraitsResource current t6
]

{ #category : 'accessing' }
TraitsTestCase >> t8 [
	^TraitsResource current t8
]

{ #category : 'accessing' }
TraitsTestCase >> t9 [
	^TraitsResource current t9
]

{ #category : 'running' }
TraitsTestCase >> tearDown [

	TraitsResource reset.

	self packageOrganizer removePackage: self packageNameForTests.

	super tearDown
]
