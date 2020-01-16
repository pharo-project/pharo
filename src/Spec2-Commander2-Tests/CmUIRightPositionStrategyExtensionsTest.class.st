"
A SpSetButtonRightStrategyTest is a test class for testing the behavior of SpSetButtonRightStrategy
"
Class {
	#name : #CmUIRightPositionStrategyExtensionsTest,
	#superclass : #TestCase,
	#category : #'Spec2-Commander2-Tests'
}

{ #category : #test }
CmUIRightPositionStrategyExtensionsTest >> testAddButtonToActionBar [
	| actionBar button |
	actionBar := SpActionBarPresenter new.
	button := SpButtonPresenter new.
	
	self assertEmpty: actionBar items.
	
	CmUIRightPositionStrategy new
		addButton: button toActionBar: actionBar.
		
	self assert: actionBar items size equals: 1.
	self assert: (actionBar items includesKey: #end).
	self assert: (actionBar items at: #end) size equals: 1.
	self assert: (actionBar items at: #end) anyOne equals: button
]

{ #category : #test }
CmUIRightPositionStrategyExtensionsTest >> testAddButtonToToolbar [
	| toolbar button |
	toolbar := SpToolBarPresenter new.
	button := SpToolBarButton new.
	
	self assertEmpty: toolbar items.
	
	CmUIRightPositionStrategy new
		addButton: button toToolbar: toolbar.
		
	self assert: toolbar rightItems size equals: 1.
	self assert: toolbar rightItems anyOne equals: button.
	self assertEmpty: toolbar leftItems
]
