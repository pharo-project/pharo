Class {
	#name : #SpTreeTableAdapterSingleColumnMultiSelectionTest,
	#superclass : #SpAbstractTreeTableAdapterTest,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #running }
SpTreeTableAdapterSingleColumnMultiSelectionTest >> initializeTestedInstance [ 

	presenter
		addColumn: (SpStringTableColumn title: 'Value' evaluated: #printString);
		hideColumnHeaders;	
		beMultipleSelection;
		roots: #(1 2 3);
		children: [ :aNumber | { aNumber * 2. aNumber * 3. aNumber * 10 } ].

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
