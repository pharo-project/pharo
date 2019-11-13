Class {
	#name : #SpAbstractTreeTableAdapterTest,
	#superclass : #SpAbstractWidgetAdapterTest,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #testing }
SpAbstractTreeTableAdapterTest class >> isAbstract [ 
	
	^ self = SpAbstractTreeTableAdapterTest
]

{ #category : #accessing }
SpAbstractTreeTableAdapterTest >> classToTest [

	^ SpTreeTablePresenter 
]

{ #category : #running }
SpAbstractTreeTableAdapterTest >> testChangingRootsResetsSelection [

	self presenter selection selectPath: #(2 3).
	self presenter roots: #(10 11 12).
	
	self assert: self adapter selectedItem equals: nil

]

{ #category : #running }
SpAbstractTreeTableAdapterTest >> testChangingRootsUpdateWidget [

	self presenter roots: #(10 11 12).
	
	backendForTest assertList: self adapter displayItems: #('10' '11' '12').
	

]

{ #category : #running }
SpAbstractTreeTableAdapterTest >> testRightClickShowsMenu [
	| menu |

	presenter contextMenu: (menu := SpMockMenuPresenter new).
	self deny: menu shown.
	self emulateRightClick.
	self assert: menu shown
]

{ #category : #running }
SpAbstractTreeTableAdapterTest >> testSelectItemRaiseEvent [

	| called |
	
	self presenter whenSelectionChangedDo: [ called := true ].
	self presenter selection selectPath: #(2 3).

	self assert: called equals: true
]

{ #category : #running }
SpAbstractTreeTableAdapterTest >> testSelectItemSelectsTheGivenElement [

	self presenter selection selectPath: #(2).
	self assert: self adapter selectedItem equals: 2.
]

{ #category : #running }
SpAbstractTreeTableAdapterTest >> testSelectItemSelectsTheGivenElementAndExpandsTheBranches [

	self presenter selection selectPath: #(2 3).
	self assert: self adapter selectedItem equals: 20.
]

{ #category : #running }
SpAbstractTreeTableAdapterTest >> testSelectingAPathUpdatesSelectionInPresenter [

	self presenter selection selectPath: #(2 3).
	self assert: self presenter selectedItem equals: 20.
]

{ #category : #running }
SpAbstractTreeTableAdapterTest >> testSelectingOutOfRangeKeepOldSelection [
	
	self presenter selection selectPath: #(2 3).
	self presenter selection selectPath: #(7 3).
	self assert: self adapter selectedItem equals: 20

]

{ #category : #running }
SpAbstractTreeTableAdapterTest >> testShowingColumnDisplayColumn [

	self presenter showColumnHeaders.
	
	backendForTest assertHasHeader: self adapter
]

{ #category : #running }
SpAbstractTreeTableAdapterTest >> testStartsWithoutSelection [

	self assert: self adapter selectedItem equals: nil.
]
