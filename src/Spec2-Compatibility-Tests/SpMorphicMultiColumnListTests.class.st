Class {
	#name : #SpMorphicMultiColumnListTests,
	#superclass : #SpMorphicAdapterTestCase,
	#category : #'Spec2-Compatibility-Tests'
}

{ #category : #running }
SpMorphicMultiColumnListTests >> classToTest [

	^ SpMultiColumnListPresenter
]

{ #category : #tests }
SpMorphicMultiColumnListTests >> testColumnsAreDynamicallySetInPresenter [

	self presenter
		displayBlock: [ :item | { item . 42 } "2 elements for 2 columns" ];
		items: { 1 . 2 . 3 }.
	self assert: self presenter columns size equals: 2
]

{ #category : #tests }
SpMorphicMultiColumnListTests >> testColumnsAreDynamicallySetInWidget [

	self presenter
		displayBlock: [ :item | { item . 42 } "2 elements for 2 columns" ];
		items: { 1 . 2 . 3 }.
	self assert: self widget columns size equals: 2
]
