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
