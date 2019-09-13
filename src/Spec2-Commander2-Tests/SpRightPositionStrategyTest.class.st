"
A SpSetButtonRightStrategyTest is a test class for testing the behavior of SpSetButtonRightStrategy
"
Class {
	#name : #SpRightPositionStrategyTest,
	#superclass : #TestCase,
	#category : #'Spec2-Commander2-Tests'
}

{ #category : #test }
SpRightPositionStrategyTest >> testAddButtonToActionBar [
	| actionBar button |
	actionBar := SpActionBarPresenter new.
	button := SpButtonPresenter new.
	
	self assertEmpty: actionBar items.
	
	SpRightPositionStrategy new
		addButton: button toActionBar: actionBar.
		
	self assert: actionBar items size equals: 1.
	self assert: (actionBar items includesKey: #end).
	self assert: (actionBar items at: #end) size equals: 1.
	self assert: (actionBar items at: #end) anyOne equals: button
]

{ #category : #test }
SpRightPositionStrategyTest >> testAddButtonToToolbar [
	| toolbar button |
	toolbar := SpToolBarPresenter new.
	button := SpToolBarButton new.
	
	self assertEmpty: toolbar items.
	
	SpRightPositionStrategy new
		addButton: button toToolbar: toolbar.
		
	self assert: toolbar rightItems size equals: 1.
	self assert: toolbar rightItems anyOne equals: button.
	self assertEmpty: toolbar leftItems
]
