Class {
	#name : #MorphicMultiColumnListTest,
	#superclass : #MorphicAdapterTestCase,
	#category : #'Spec-CompatibilityTests'
}

{ #category : #running }
MorphicMultiColumnListTest >> classToTest [

	^ MultiColumnListPresenter
]

{ #category : #tests }
MorphicMultiColumnListTest >> testColumnsAreDynamicallySetInPresenter [

	self presenter
		displayBlock: [ :item | { item . 42 } "2 elements for 2 columns" ];
		items: { 1 . 2 . 3 }.
	self assert: self presenter columns size equals: 2
]

{ #category : #tests }
MorphicMultiColumnListTest >> testColumnsAreDynamicallySetInWidget [

	self presenter
		displayBlock: [ :item | { item . 42 } "2 elements for 2 columns" ];
		items: { 1 . 2 . 3 }.
	self assert: self widget columns size equals: 2
]
