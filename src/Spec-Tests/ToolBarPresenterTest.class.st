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
