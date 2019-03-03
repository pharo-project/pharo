Class {
	#name : #MorphicMultiColumnListTests,
	#superclass : #MorphicAdapterTestCase,
	#category : #'Spec-CompatibilityTests'
}

{ #category : #running }
MorphicMultiColumnListTests >> classToTest [

	^ MultiColumnListPresenter
]

{ #category : #tests }
MorphicMultiColumnListTests >> testColumnsAreDynamicallySetInPresenter [

	self presenter
		displayBlock: [ :item | { item . 42 } "2 elements for 2 columns" ];
		items: { 1 . 2 . 3 }.
	self assert: self presenter columns size equals: 2
]

{ #category : #tests }
MorphicMultiColumnListTests >> testColumnsAreDynamicallySetInWidget [

	self presenter
		displayBlock: [ :item | { item . 42 } "2 elements for 2 columns" ];
		items: { 1 . 2 . 3 }.
	self assert: self widget columns size equals: 2
]
