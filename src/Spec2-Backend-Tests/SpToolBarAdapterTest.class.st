Class {
	#name : #SpToolBarAdapterTest,
	#superclass : #SpAbstractWidgetAdapterTest,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #accessing }
SpToolBarAdapterTest >> classToTest [
	^ SpToolBarPresenter 
]

{ #category : #running }
SpToolBarAdapterTest >> initializeTestedInstance [
	presenter 

]

{ #category : #tests }
SpToolBarAdapterTest >> testAddLabelledButtonAddTheButton [
	presenter addItem: (SpToolBarButton new
				label: 'a label';
				iconNamed: #abstract;
				action: [];
				yourself).
				
	self assert: (self adapter hasButtonWithLabel: 'a label').
	self assert: (self adapter numberOfItems) equals: 1
]
