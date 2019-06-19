Class {
	#name : #AbstractTreeTableAdapterTest,
	#superclass : #AbstractWidgetAdapterTestCase,
	#category : #'Spec-BackendTests'
}

{ #category : #testing }
AbstractTreeTableAdapterTest class >> isAbstract [ 
	
	^ self = AbstractTreeTableAdapterTest
]

{ #category : #accessing }
AbstractTreeTableAdapterTest >> classToTest [

	^ TreeTablePresenter 
]

{ #category : #running }
AbstractTreeTableAdapterTest >> testChangingRootsResetsSelection [

	self presenter selection selectPath: #(2 3).
	self presenter roots: #(10 11 12).
	
	self assert: self adapter selectedItem equals: nil

]

{ #category : #running }
AbstractTreeTableAdapterTest >> testChangingRootsUpdateWidget [

	self presenter roots: #(10 11 12).
	
	backendForTest assertList: self adapter widget displayItems: #('10' '11' '12').
	

]

{ #category : #running }
AbstractTreeTableAdapterTest >> testSelectItemRaiseEvent [

	| called |
	
	self presenter whenSelectionChangedDo: [ called := true ].
	self presenter selection selectPath: #(2 3).

	self assert: called equals: true
]

{ #category : #running }
AbstractTreeTableAdapterTest >> testSelectItemSelectsTheGivenElement [

	self presenter selection selectPath: #(2).
	self assert: self adapter selectedItem equals: 2.
]

{ #category : #running }
AbstractTreeTableAdapterTest >> testSelectItemSelectsTheGivenElementAndExpandsTheBranches [

	self presenter selection selectPath: #(2 3).
	self assert: self adapter selectedItem equals: 20.
]

{ #category : #running }
AbstractTreeTableAdapterTest >> testSelectingAPathUpdatesSelectionInPresenter [

	self presenter selection selectPath: #(2 3).
	self assert: self presenter selectedItem equals: 20.
]

{ #category : #running }
AbstractTreeTableAdapterTest >> testSelectingOutOfRangeKeepOldSelection [
	
	self presenter selection selectPath: #(2 3).
	self presenter selection selectPath: #(7 3).
	self assert: self adapter selectedItem equals: 20

]

{ #category : #running }
AbstractTreeTableAdapterTest >> testShowingColumnDisplayColumn [

	self presenter showColumnHeaders.
	
	backendForTest assertHasHeader: self adapter widget
]

{ #category : #running }
AbstractTreeTableAdapterTest >> testStartsWithoutSelection [

	self assert: self adapter selectedItem equals: nil.
]
