Class {
	#name : #SpFastTableTest,
	#superclass : #SpMorphicAdapterTestCase,
	#category : #'Spec2-Deprecated-Tests'
}

{ #category : #running }
SpFastTableTest >> classToTest [

	^ SpFastTablePresenter
]

{ #category : #running }
SpFastTableTest >> initializeTestedInstance [

	presenter
		numberOfColumns: 2;
		displayBlock: [ :item | { item . 42 . 1 } ];
		items: { 1 . 2 . 3 }
]

{ #category : #running }
SpFastTableTest >> testColumnsAreExplicitlySetEvenIfDisplayBlockContainsMoreItems [

	self assert: presenter columns size equals: 2
]
