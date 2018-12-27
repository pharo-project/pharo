"
I'm a presenter to render a toolbar.
I can receive any kind of ToolBarItemPresenters, even if the most frequent one is ToolBarButton.

Toolbars can be displayed in diferent modes: 
- icons and labels (default)
- icons
- labels

And they can be placed at the left (default) or at the right. 
Not all platforms support left/right alignments for toolbars. In that case they will be presented as simple groups.
"
Class {
	#name : #ToolBarPresenter,
	#superclass : #AbstractWidgetPresenter,
	#instVars : [
		'items',
		'displayMode'
	],
	#category : #'Spec-Core-Widgets-Toolbar'
}

{ #category : #specs }
ToolBarPresenter class >> adapterName [

	^ #ToolBarAdapter
]

{ #category : #adding }
ToolBarPresenter >> addItem: aToolbarItem [
	
	self addItem: aToolbarItem position: ToolBarItemPosition left
]

{ #category : #adding }
ToolBarPresenter >> addItem: aToolbarItem position: aPosition [

	(items 
		at: aPosition
		ifAbsentPut: [ OrderedCollection new ])
		add: aToolbarItem
]

{ #category : #accessing }
ToolBarPresenter >> beBoth [

	displayMode := ToolBarDisplayMode modeIconAndLabel
]

{ #category : #accessing }
ToolBarPresenter >> beIcons [

	displayMode := ToolBarDisplayMode modeIcon
]

{ #category : #accessing }
ToolBarPresenter >> beText [

	displayMode := ToolBarDisplayMode modeLabel
]

{ #category : #accessing }
ToolBarPresenter >> displayMode [

	^ displayMode
]

{ #category : #initialization }
ToolBarPresenter >> initialize [

	super initialize.
	self beBoth.
	items := Dictionary new asValueHolder.
	
]

{ #category : #testing }
ToolBarPresenter >> isDisplayModeBoth [

	^ self displayMode = ToolBarDisplayMode modeIconAndLabel
]

{ #category : #testing }
ToolBarPresenter >> isDisplayModeIcons [

	^ self displayMode = ToolBarDisplayMode modeIcon.
]

{ #category : #testing }
ToolBarPresenter >> isDisplayModeText [

	^ self displayMode = ToolBarDisplayMode modeLabel
]

{ #category : #testing }
ToolBarPresenter >> isEmpty [
	
	^ self items isEmpty
]

{ #category : #api }
ToolBarPresenter >> items [
	^ items value
]

{ #category : #testing }
ToolBarPresenter >> notEmpty [

	^ self isEmpty not
]
