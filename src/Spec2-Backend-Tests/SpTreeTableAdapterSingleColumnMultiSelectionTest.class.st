Class {
	#name : #SpTreeTableAdapterSingleColumnMultiSelectionTest,
	#superclass : #SpAbstractTreeTableAdapterTest,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #running }
SpTreeTableAdapterSingleColumnMultiSelectionTest >> initializeTestedInstance [
	super initializeTestedInstance.
	presenter beMultipleSelection
]

{ #category : #running }
SpTreeTableAdapterSingleColumnMultiSelectionTest >> testSelecting2ElementsUpdatesThePresenterCorrectly [


]

{ #category : #running }
SpTreeTableAdapterSingleColumnMultiSelectionTest >> testSelectingAPathUpdatesSelectionInPresenter [

	self presenter selection 
		selectPath: #(1 2);
		selectPath: #(2 3).
		
	self assert: self adapter selectedPaths asArray equals: #(#(1 2) (2 3))
]
