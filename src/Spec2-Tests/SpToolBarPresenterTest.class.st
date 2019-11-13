Class {
	#name : #SpToolBarPresenterTest,
	#superclass : #SpTest,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #running }
SpToolBarPresenterTest >> classToTest [

	^ SpToolBarPresenter
]

{ #category : #private }
SpToolBarPresenterTest >> newToolbarItem [
	
	^ SpToolBarButton new
]

{ #category : #tests }
SpToolBarPresenterTest >> testAddItem [

	presenter addItem: SpToolBarButton new.
	self assert: presenter items size equals: 1
]

{ #category : #tests }
SpToolBarPresenterTest >> testAddItemPosition [
	| itemLeft itemRight |

	self assert: presenter items isEmpty.
	presenter addItem: (itemLeft := self newToolbarItem) position: SpToolBarItemPosition left.
	presenter addItem: (itemRight := self newToolbarItem) position: SpToolBarItemPosition right.

	self assertCollection: presenter leftItems hasSameElements: { itemLeft }.
	self assertCollection: presenter rightItems hasSameElements: { itemRight }
]

{ #category : #tests }
SpToolBarPresenterTest >> testDisplayMode [

	presenter beBoth.
	self assert: presenter isDisplayModeBoth.
	presenter beIcons.
	self assert: presenter isDisplayModeIcons.	
	presenter beText.
	self assert: presenter isDisplayModeText
	
]

{ #category : #tests }
SpToolBarPresenterTest >> testIsEmpty [ 

	self assert: presenter isEmpty.
	presenter addItem: SpToolBarButton new.
	self deny: presenter isEmpty
]

{ #category : #tests }
SpToolBarPresenterTest >> testItems [ 
	| item |

	self assert: presenter items isEmpty.
	presenter addItem: (item := self newToolbarItem).

	self assertCollection: presenter leftItems hasSameElements: { item }
]
