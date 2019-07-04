Class {
	#name : #SpFastTableTests,
	#superclass : #SpMorphicAdapterTestCase,
	#category : #'Spec2-Compatibility-Tests'
}

{ #category : #running }
SpFastTableTests >> classToTest [

	^ SpFastTablePresenter
]

{ #category : #running }
SpFastTableTests >> initializeTestedInstance [

	presenter
		numberOfColumns: 2;
		displayBlock: [ :item | { item . 42 . 1 } ];
		items: { 1 . 2 . 3 }
]

{ #category : #running }
SpFastTableTests >> testColumnsAreExplicitlySetEvenIfDisplayBlockContainsMoreItems [

	self assert: presenter columns size equals: 2
]
