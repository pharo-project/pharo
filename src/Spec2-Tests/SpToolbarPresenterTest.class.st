Class {
	#name : #SpToolbarPresenterTest,
	#superclass : #SpSpecTest,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #running }
SpToolbarPresenterTest >> classToTest [

	^ SpToolbarPresenter
]

{ #category : #private }
SpToolbarPresenterTest >> newToolbarItem [
	
	^ SpToolbarButtonPresenter new
]

{ #category : #tests }
SpToolbarPresenterTest >> testAddItem [

	presenter addItem: SpToolbarButtonPresenter new.
	self assert: presenter items size equals: 1
]

{ #category : #tests }
SpToolbarPresenterTest >> testAddItemPosition [
	| itemLeft itemRight |

	self assert: presenter items isEmpty.
	presenter addItem: (itemLeft := self newToolbarItem) position: SpToolbarItemPosition left.
	presenter addItem: (itemRight := self newToolbarItem) position: SpToolbarItemPosition right.

	self assertCollection: presenter leftItems hasSameElements: { itemLeft }.
	self assertCollection: presenter rightItems hasSameElements: { itemRight }
]

{ #category : #tests }
SpToolbarPresenterTest >> testDisplayMode [

	presenter beBoth.
	self assert: presenter isDisplayModeBoth.
	presenter beIcons.
	self assert: presenter isDisplayModeIcons.	
	presenter beText.
	self assert: presenter isDisplayModeText
	
]

{ #category : #tests }
SpToolbarPresenterTest >> testIsEmpty [ 

	self assert: presenter isEmpty.
	presenter addItem: SpToolbarButtonPresenter new.
	self deny: presenter isEmpty
]

{ #category : #tests }
SpToolbarPresenterTest >> testItems [ 
	| item |

	self assert: presenter items isEmpty.
	presenter addItem: (item := self newToolbarItem).

	self assertCollection: presenter leftItems hasSameElements: { item }
]
