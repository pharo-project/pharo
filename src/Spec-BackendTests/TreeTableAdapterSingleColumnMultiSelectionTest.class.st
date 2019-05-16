Class {
	#name : #TreeTableAdapterSingleColumnMultiSelectionTest,
	#superclass : #AbstractTreeTableAdapterTest,
	#category : #'Spec-BackendTests'
}

{ #category : #running }
TreeTableAdapterSingleColumnMultiSelectionTest >> initializeTestedInstance [ 

	presenter
		addColumn: (StringTableColumn title: 'Value' evaluated: #printString);
		hideColumnHeaders;	
		beMultipleSelection;
		roots: #(1 2 3);
		children: [ :aNumber | { aNumber * 2. aNumber * 3. aNumber * 10 } ].

]

{ #category : #running }
TreeTableAdapterSingleColumnMultiSelectionTest >> testSelecting2ElementsUpdatesThePresenterCorrectly [


]

{ #category : #running }
TreeTableAdapterSingleColumnMultiSelectionTest >> testSelectingAPathUpdatesSelectionInPresenter [

	self presenter selection 
		selectPath: #(1 2);
		selectPath: #(2 3).
		
	self assert: self adapter selectedPaths asArray equals: #(#(1 2) (2 3))
]
