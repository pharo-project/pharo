Class {
	#name : #SpDropListWithoutInitialSelectionAdapterTest,
	#superclass : #SpDropListAdapterTest,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #accessing }
SpDropListWithoutInitialSelectionAdapterTest >> initializeTestedInstance [ 

	presenter startWithoutSelection.	
	super initializeTestedInstance.

]

{ #category : #accessing }
SpDropListWithoutInitialSelectionAdapterTest >> testAddingAnItemSelectsItIfThereIsNone [

	self presenter addItemLabeled: 40 do: [].

	self assert: self presenter selectedItem equals: 40.
	self assert: self adapter selectedIndexes equals: #(4).
	
]

{ #category : #accessing }
SpDropListWithoutInitialSelectionAdapterTest >> testSelectionStartsEmptyIfStartWithoutSelectionIsSet [

	self assert: self presenter selectedItem equals: nil.
	self assert: self adapter selectedIndexes equals: #().
	
]
