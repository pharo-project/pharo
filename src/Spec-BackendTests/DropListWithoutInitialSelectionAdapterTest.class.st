Class {
	#name : #DropListWithoutInitialSelectionAdapterTest,
	#superclass : #DropListAdapterTest,
	#category : #'Spec-BackendTests'
}

{ #category : #accessing }
DropListWithoutInitialSelectionAdapterTest >> initializeTestedInstance [ 

	presenter startWithoutSelection.	
	super initializeTestedInstance.

]

{ #category : #accessing }
DropListWithoutInitialSelectionAdapterTest >> testAddingAnItemSelectsItIfThereIsNone [

	self presenter addItemLabeled: 40 do: [].

	self assert: self presenter selectedItem equals: 40.
	self assert: self adapter selectedIndexes equals: #(4).
	
]

{ #category : #accessing }
DropListWithoutInitialSelectionAdapterTest >> testSelectionStartsEmptyIfStartWithoutSelectionIsSet [

	self assert: self presenter selectedItem equals: nil.
	self assert: self adapter selectedIndexes equals: #().
	
]
