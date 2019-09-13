Class {
	#name : #SpMorphicMultiColumnListTest,
	#superclass : #SpMorphicAdapterTestCase,
	#category : #'Spec2-Deprecated-Tests'
}

{ #category : #running }
SpMorphicMultiColumnListTest >> classToTest [

	^ SpMultiColumnListPresenter
]

{ #category : #tests }
SpMorphicMultiColumnListTest >> testColumnsAreDynamicallySetInPresenter [

	self presenter
		displayBlock: [ :item | { item . 42 } "2 elements for 2 columns" ];
		items: { 1 . 2 . 3 }.
	self assert: self presenter columns size equals: 2
]

{ #category : #tests }
SpMorphicMultiColumnListTest >> testColumnsAreDynamicallySetInWidget [

	self presenter
		displayBlock: [ :item | { item . 42 } "2 elements for 2 columns" ];
		items: { 1 . 2 . 3 }.
	self assert: self widget columns size equals: 2
]
