Class {
	#name : #FastTableTest,
	#superclass : #MorphicAdapterTestCase,
	#category : #'Spec-CompatibilityTests'
}

{ #category : #running }
FastTableTest >> classToTest [

	^ FastTablePresenter
]

{ #category : #running }
FastTableTest >> initializeTestedInstance [

	presenter
		numberOfColumns: 2;
		displayBlock: [ :item | { item . 42 . 1 } ];
		items: { 1 . 2 . 3 }
]

{ #category : #running }
FastTableTest >> testColumnsAreExplicitlySetEvenIfDisplayBlockContainsMoreItems [

	self assert: presenter columns size equals: 2
]
