"
testing ListComposablePresenter
"
Class {
	#name : #AbstractListPresenterTest,
	#superclass : #SpecTestCase,
	#category : #'Spec-Tests-Core-Widgets'
}

{ #category : #testing }
AbstractListPresenterTest class >> isAbstract [

	^ self ==  AbstractListPresenterTest
]

{ #category : #running }
AbstractListPresenterTest >> setUp [

	super setUp.
	presenter items: #(10 20 30).
]

{ #category : #'tests-activation' }
AbstractListPresenterTest >> testActivationOnDoubleClickShouldActivateOnDoubleClick [

	| activatedItem |
	activatedItem := nil.
	presenter
		activateOnDoubleClick;
		whenActivatedDo: [ :selection | activatedItem := selection selectedItem ].
	
	presenter doubleClickAtIndex: 1.
	
	self assert: activatedItem equals: 10.
]

{ #category : #'tests-activation' }
AbstractListPresenterTest >> testActivationOnDoubleClickShouldNotActivateOnClick [

	| activatedItem |
	activatedItem := nil.
	presenter
		activateOnDoubleClick;
		whenActivatedDo: [ :selection  | activatedItem := selection selectionItem ].
	
	presenter clickAtIndex: 1.
	
	self assert: activatedItem equals: nil.
]

{ #category : #'tests-activation' }
AbstractListPresenterTest >> testActivationOnSingleClickShouldActivateOnClick [

	| activatedItem |
	activatedItem := nil.
	presenter
		activateOnSingleClick;
		whenActivatedDo: [ :selection | activatedItem := selection selectedItem ].
	
	presenter clickAtIndex: 1.
	
	self assert: activatedItem equals: 10.
]

{ #category : #'tests-activation' }
AbstractListPresenterTest >> testActivationOnSingleClickShouldNotActivateOnClickOutside [

	| activatedItem |
	activatedItem := nil.
	presenter
		activateOnSingleClick;
		whenActivatedDo: [ :selection | activatedItem := selection selectedIndex ].
	
	presenter clickAtIndex: 100.
	
	self assert: activatedItem equals: nil
]

{ #category : #'tests-activation' }
AbstractListPresenterTest >> testActivationWithoutActivationBlockDoesNothing [

	| activatedItem |
	activatedItem := nil.
	presenter activateOnSingleClick.

	presenter clickAtIndex: 1.
	
	self assert: activatedItem equals: nil
]

{ #category : #tests }
AbstractListPresenterTest >> testContextMenu [
	| menu changed |
	
	self assert: presenter contextMenu isNil.
	menu := MenuPresenter new.
	changed := false.
	presenter whenMenuChangedDo: [ 
		changed := true. ].
	presenter contextMenu: menu.
	self assert: presenter contextMenu equals: menu.
	self assert: changed
]

{ #category : #tests }
AbstractListPresenterTest >> testReplaceItemList [
	| changed |

	changed := false.
	presenter whenModelChangedDo: [ changed := true ].
	presenter items: #(a b c).
	
	self 
		assert: presenter model collection
		equals: #(a b c).
	self assert: changed
]

{ #category : #tests }
AbstractListPresenterTest >> testSelectAll [

	presenter beSingleSelection.
	presenter selectAll.
	"in single mode selectAll has no effect"
	self assertEmpty: presenter selection selectedItems. 
	presenter beMultipleSelection.
	presenter selectAll.
	"but in multiple mode selectAll works"
	self 
		assert: presenter selection selectedItems asArray 
		equals: #(10 20 30)
]

{ #category : #'tests-smoke' }
AbstractListPresenterTest >> testSetSortingBlockBeforeItems [
	| count |
	count := 0.
	presenter whenSortingBlockChangedDo: [ :sortFunction | count := count + 1 ].
	presenter sortingBlock: #yourself ascending.
	presenter items: #(3 8 1 0).
	self assert: count equals: 1.
	self assert: (presenter model at: 1) equals: 0
]

{ #category : #'tests-smoke' }
AbstractListPresenterTest >> testSmokeOpenEmptyPresenter [

	window := presenter openWithSpec
]

{ #category : #'tests-smoke' }
AbstractListPresenterTest >> testSmokeOpenPresenterWithItems [

	window := presenter
		items: #(1 2 3);
		openWithSpec.
]

{ #category : #'tests-smoke' }
AbstractListPresenterTest >> testSortingBlock [
	| count |
	count := 0.
	presenter whenSortingBlockChangedDo: [ :sortFunction | count := count + 1 ].
	presenter items: #(3 8 1 0).
	presenter sortingBlock: #yourself ascending.
	self assert: count equals: 1.
	self assert: (presenter model at: 1) equals: 0
]

{ #category : #tests }
AbstractListPresenterTest >> testUnselectAll [

	presenter beMultipleSelection.
	presenter selectAll.
	self 
		assert: presenter selection selectedItems asArray 
		equals: #(10 20 30).
	presenter unselectAll.
	self assertEmpty: presenter selection selectedItems
]
