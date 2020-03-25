Class {
	#name : #SpEditableListPresenterTest,
	#superclass : #SpSmokeTest,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #running }
SpEditableListPresenterTest >> classToTest [
	^ SpEditableListPresenter
]

{ #category : #tests }
SpEditableListPresenterTest >> testCanAddNewItem [
	presenter 
		items: #(1 2 3) asOrderedCollection;
		addItemBlock: [ 4 ].
		
	presenter addButton click.
	
	self 
		assertCollection: presenter items
		hasSameElements: #(1 2 3 4)
]

{ #category : #tests }
SpEditableListPresenterTest >> testCanCancelAddNewItem [
	presenter 
		items: #(1 2 3) asOrderedCollection;
		addItemBlock: [ nil ].
		
	presenter addButton click.
	
	self 
		assertCollection: presenter items
		hasSameElements: #(1 2 3)
]

{ #category : #tests }
SpEditableListPresenterTest >> testCanRemoveSelectedItem [
	presenter 
		items: #(1 2 3) asOrderedCollection;
		selectItem: 2.
	
	presenter removeButton click.
	
	self 
		assertCollection: presenter items
		hasSameElements: #(1 3)
]

{ #category : #tests }
SpEditableListPresenterTest >> testMoveElementAtTo [
	presenter items: {'AAA' . 'BBB' . 'CCC'} asOrderedCollection.
	presenter moveElementAt: 1 to: 3.
	self
		assert: presenter items asArray
		equals: {'BBB' . 'CCC' . 'AAA'}
]

{ #category : #tests }
SpEditableListPresenterTest >> testRemoveBlockExecutedWhenSelectedItemRemoved [
	| executed selectedItem |
	executed := false.
	presenter 
		items: #(1 2 3) asOrderedCollection;
		removeItemBlock: [ :item | executed := true. selectedItem := item ];
		selectItem: 2.
	
	presenter removeButton click.
	
	self 
		assert: executed
		description: 'removeBlock not exeuted when selected item removed!'.
	self 
		assert: selectedItem
		equals: 2
]
