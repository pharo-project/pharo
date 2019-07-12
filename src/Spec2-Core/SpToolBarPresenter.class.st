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
	#name : #SpToolBarPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#instVars : [
		'displayMode',
		'itemsHolder'
	],
	#category : #'Spec2-Core-Widgets-Toolbar'
}

{ #category : #specs }
SpToolBarPresenter class >> adapterName [

	^ #ToolBarAdapter
]

{ #category : #convenience }
SpToolBarPresenter >> addButtonLabel: aLabel iconNamed: anIconName help: aHelpString action: aBlock [

	self addItem: (SpToolBarButton new
		label: aLabel;
		icon: (self application iconNamed: anIconName);
		help: aHelpString;
		action: aBlock;
		yourself)
]

{ #category : #adding }
SpToolBarPresenter >> addItem: aToolbarItem [
	
	self addItem: aToolbarItem position: SpToolBarItemPosition left
]

{ #category : #adding }
SpToolBarPresenter >> addItem: aToolbarItem position: aPosition [
	| wasEmpty |
	
	aToolbarItem owner: self.
	wasEmpty := false.
	(itemsHolder 
		at: aPosition
		ifAbsentPut: [ 
			wasEmpty := true.
			OrderedCollection new ])
		add: aToolbarItem.
	
	"Triggering add messages.
 	 This is horrible, items holder should be a collection not a dictionary and constraints (like
	 group) should be handled differently"
	wasEmpty ifFalse: [ 
		itemsHolder valueChanged: aPosition ]
]

{ #category : #accessing }
SpToolBarPresenter >> allItems [
	
	^ self items values flattened
]

{ #category : #accessing }
SpToolBarPresenter >> beBoth [

	displayMode := SpToolBarDisplayMode modeIconAndLabel
]

{ #category : #accessing }
SpToolBarPresenter >> beIcons [

	displayMode := SpToolBarDisplayMode modeIcon
]

{ #category : #accessing }
SpToolBarPresenter >> beText [

	displayMode := SpToolBarDisplayMode modeLabel
]

{ #category : #accessing }
SpToolBarPresenter >> displayMode [

	^ displayMode
]

{ #category : #initialization }
SpToolBarPresenter >> initialize [

	super initialize.
	self beBoth.
	itemsHolder := Dictionary new asValueHolder.
	
]

{ #category : #testing }
SpToolBarPresenter >> isDisplayModeBoth [

	^ self displayMode = SpToolBarDisplayMode modeIconAndLabel
]

{ #category : #testing }
SpToolBarPresenter >> isDisplayModeIcons [

	^ self displayMode = SpToolBarDisplayMode modeIcon
]

{ #category : #testing }
SpToolBarPresenter >> isDisplayModeText [

	^ self displayMode = SpToolBarDisplayMode modeLabel
]

{ #category : #testing }
SpToolBarPresenter >> isEmpty [
	
	^ self items isEmpty
]

{ #category : #accessing }
SpToolBarPresenter >> itemNamed: aString [ 
	
	^ self allItems detect: [ :e | e label = aString ]
]

{ #category : #api }
SpToolBarPresenter >> items [

	^ itemsHolder value
]

{ #category : #testing }
SpToolBarPresenter >> notEmpty [

	^ self isEmpty not
]
