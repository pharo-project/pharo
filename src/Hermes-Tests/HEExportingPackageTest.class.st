Class {
	#name : #HEExportingPackageTest,
	#superclass : #HEAbstractBinaryClassBuilderTest,
	#instVars : [
		'old',
		'aPackage'
	],
	#category : 'Hermes-Tests'
}

{ #category : #'as yet unclassified' }
HEExportingPackageTest >> doTestExportingPackage: aPackageNamed [
	| new |
 
	self fillOldPackage: aPackageNamed.

	old writeInto: writer.
	writer flush.
	
	new := HEPackage readFrom: reader.
	
	self assert: old classes size equals: (aPackage definedClasses reject:#isTrait) size.
	self assert: old traits size equals: (aPackage definedClasses select:#isTrait) size.

	self assert: old extensionMethods size equals: (aPackage extensionMethods) size.
	
	self assert: ( old classes allSatisfy: [ :e | self in: old classes allSuperclassesAreBeforeThan: e] ).
	self assert: ( old classes allSatisfy: [ :e | self in: old classes allSharedPoolsAreBeforeThan: e] ).
	self assert: ( old traits allSatisfy: [ :e | self in: old traits allTraitsAreBeforeThan: e] ).

	old traits with: new traits do: [ :a :b | self assert: a equals:b ].
	old classes with: new classes do: [ :a :b | self assert: a equals:b ].
	old extensionMethods with: new extensionMethods do: [ :a :b | self assert: a equals:b ].

	self assert: new equals: old.
]

{ #category : #'as yet unclassified' }
HEExportingPackageTest >> fillOldPackage: aPackageNamed [
	aPackage := RPackageOrganizer default packageNamed: aPackageNamed.
	old := HEPackage for:aPackage.

]

{ #category : #'as yet unclassified' }
HEExportingPackageTest >> in: aSeq allSharedPoolsAreBeforeThan: aClass [
	| myIndex allSharedPools |
	myIndex := aSeq indexOf: aClass.
	allSharedPools := ({aClass} , (aClass allSuperclassesIn: aSeq)) flatCollect: [:e | e sharedPoolsIn: aSeq].
	
	^ allSharedPools allSatisfy: [ :sp | (aSeq indexOf: sp) < myIndex ]
]

{ #category : #'as yet unclassified' }
HEExportingPackageTest >> in: aSeq allSuperclassesAreBeforeThan: aClass [
	| myIndex superclass superclassIndex |
	myIndex := aSeq indexOf: aClass.
	superclass := aSeq detect: [ :e | e className = aClass superclass ] ifNone: [ ^ true ].

	superclassIndex := aSeq indexOf: superclass.
	^ (superclassIndex < myIndex) and:[ self in:aSeq allSuperclassesAreBeforeThan: superclass].
]

{ #category : #'as yet unclassified' }
HEExportingPackageTest >> in: aSeq allTraitsAreBeforeThan: aTrait [
	| myIndex |
	myIndex := aSeq indexOf: aTrait.
	^ (aTrait traitComposition allDependenciesIn: aSeq) allSatisfy: [ :aUsedTrait | (aSeq indexOf: aUsedTrait) < myIndex ]
]

{ #category : #tests }
HEExportingPackageTest >> testExportingCollectionsTests [
	self doTestExportingPackage: 'Collections-Tests'.

]

{ #category : #tests }
HEExportingPackageTest >> testExportingKernel [
	self doTestExportingPackage: 'Kernel'.

]

{ #category : #tests }
HEExportingPackageTest >> testExportingKernelTests [
	self doTestExportingPackage: 'Kernel-Tests'.

]

{ #category : #tests }
HEExportingPackageTest >> testExportingOpal [
	self doTestExportingPackage: 'OpalCompiler-Core'.

]

{ #category : #tests }
HEExportingPackageTest >> testExportingSUnit [
		self doTestExportingPackage: 'SUnit-Core'.
		self doTestExportingPackage: 'SUnit-Tests'.

]

{ #category : #tests }
HEExportingPackageTest >> testExportingTraits [
	self doTestExportingPackage: 'Traits'.

]
