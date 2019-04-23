Class {
	#name : #ToolBarPresenterTest,
	#superclass : #SpecTestCase,
	#category : #'Spec-Tests-Core-Widgets'
}

{ #category : #running }
ToolBarPresenterTest >> classToTest [

	^ ToolBarPresenter
]

{ #category : #private }
ToolBarPresenterTest >> newToolbarItem [
	
	^ ToolBarButton new
]

{ #category : #tests }
ToolBarPresenterTest >> testAddItem [

	presenter addItem: ToolBarButton new.
	self assert: presenter items size equals: 1
]

{ #category : #tests }
ToolBarPresenterTest >> testAddItemPosition [
	| itemLeft itemRight |

	self assert: presenter items isEmpty.
	presenter addItem: (itemLeft := self newToolbarItem) position: ToolBarItemPosition left.
	presenter addItem: (itemRight := self newToolbarItem) position: ToolBarItemPosition right.
	self 
		assert: presenter items 
		equals: (Dictionary newFromPairs: { 
			ToolBarItemPosition left. { itemLeft } asOrderedCollection.
			ToolBarItemPosition right. { itemRight } asOrderedCollection })
]

{ #category : #tests }
ToolBarPresenterTest >> testDisplayMode [

	presenter beBoth.
	self assert: presenter isDisplayModeBoth.
	presenter beIcons.
	self assert: presenter isDisplayModeIcons.	
	presenter beText.
	self assert: presenter isDisplayModeText
	
]

{ #category : #tests }
ToolBarPresenterTest >> testIsEmpty [ 

	self assert: presenter isEmpty.
	presenter addItem: ToolBarButton new.
	self deny: presenter isEmpty
]

{ #category : #tests }
ToolBarPresenterTest >> testItems [ 
	| item |

	self assert: presenter items isEmpty.
	presenter addItem: (item := self newToolbarItem).
	self 
		assert: presenter items 
		equals: (Dictionary newFromPairs: { ToolBarItemPosition left. { item } asOrderedCollection })
]
