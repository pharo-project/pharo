"
A presenter to display a toolbar.

"
Class {
	#name : #SpToolbarPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#instVars : [
		'#displayMode => SpObservableSlot',
		'#leftItems => SpObservableSlot',
		'#rightItems => SpObservableSlot'
	],
	#category : #'Spec2-Core-Widgets-Toolbar'
}

{ #category : #specs }
SpToolbarPresenter class >> adapterName [

	^ #ToolbarAdapter
]

{ #category : #documentation }
SpToolbarPresenter class >> documentFactoryMethodSelector [

	^ #newToolbar
]

{ #category : #api }
SpToolbarPresenter >> add: aToolbarButtonPresenter [ 
	"Add a toolbar button to the toolbar."
	
	self addItem: aToolbarButtonPresenter
]

{ #category : #api }
SpToolbarPresenter >> addItem: aToolbarItem [
	"Add a toolbar button to the toolbar.
	 This is a synonym of `SpToolbarPresenter>>#add:`"
	
	self addItem: aToolbarItem position: SpToolbarItemPosition left
]

{ #category : #private }
SpToolbarPresenter >> addItem: aToolbarItem position: aPosition [
	
	aToolbarItem owner: self.
	aPosition addItem: aToolbarItem into: self
]

{ #category : #private }
SpToolbarPresenter >> addItemLeft: aToolbarItem [ 
	
	aToolbarItem owner: self.
	leftItems add: aToolbarItem.
	self notifyPropertyChanged: #leftItems.
]

{ #category : #private }
SpToolbarPresenter >> addItemRight: aToolbarItem [ 

	aToolbarItem owner: self.
	rightItems add: aToolbarItem.
	self notifyPropertyChanged: #rightItems.	
]

{ #category : #api }
SpToolbarPresenter >> beBoth [
	"Set the toolbar to display both text and icons.
	 See also `SpToolbarPresenter>>#displayMode:`"

	self displayMode: SpToolbarDisplayMode modeIconAndLabel
]

{ #category : #api }
SpToolbarPresenter >> beIcons [
	"Set the toolbar to display just the icons.
	See also `SpToolbarPresenter>>#displayMode:`"

	self displayMode: SpToolbarDisplayMode modeIcon
]

{ #category : #api }
SpToolbarPresenter >> beText [
	"Set the toolbar to display just the labels.
	 See also `SpToolbarPresenter>>#displayMode:`"

	self displayMode: SpToolbarDisplayMode modeLabel
]

{ #category : #api }
SpToolbarPresenter >> displayMode [
	"Answer the display mode of the toolbar (an instance of `SpToolbarDisplayMode`"

	^ displayMode
]

{ #category : #api }
SpToolbarPresenter >> displayMode: aDisplayMode [
	"Set the display mode of this toolbar. 
	 `aDisplayMode` is an instance of `SpToolbarDisplayMode`"

	displayMode ifNotNil: [ 
		self removeStyle: displayMode styleName ].
	displayMode := aDisplayMode.
	self addStyle: displayMode styleName

]

{ #category : #initialization }
SpToolbarPresenter >> initialize [

	super initialize.
	self beBoth.
	
	leftItems := OrderedCollection new.
	rightItems := OrderedCollection new.	
]

{ #category : #testing }
SpToolbarPresenter >> isDisplayModeBoth [

	^ self displayMode = SpToolbarDisplayMode modeIconAndLabel
]

{ #category : #testing }
SpToolbarPresenter >> isDisplayModeIcons [

	^ self displayMode = SpToolbarDisplayMode modeIcon
]

{ #category : #testing }
SpToolbarPresenter >> isDisplayModeText [

	^ self displayMode = SpToolbarDisplayMode modeLabel
]

{ #category : #testing }
SpToolbarPresenter >> isEmpty [
	
	^ self items isEmpty
]

{ #category : #private }
SpToolbarPresenter >> itemNamed: aString [ 
	
	^ self items detect: [ :e | e label = aString ]
]

{ #category : #api }
SpToolbarPresenter >> items [
	"Answer all buttons in the toolbar"

	^ leftItems , rightItems
]

{ #category : #private }
SpToolbarPresenter >> leftItems [
	^ leftItems
]

{ #category : #testing }
SpToolbarPresenter >> notEmpty [

	^ self isEmpty not
]

{ #category : #'private-traversing' }
SpToolbarPresenter >> presenters [

	^ self items
]

{ #category : #'private-traversing' }
SpToolbarPresenter >> presentersInFocusOrder [

	^ self items
]

{ #category : #api }
SpToolbarPresenter >> removeAllItems [
	"Remove all items in the toolbar"

	leftItems := OrderedCollection new.
	rightItems := OrderedCollection new
]

{ #category : #private }
SpToolbarPresenter >> rightItems [
	^ rightItems
]

{ #category : #'private-traversing' }
SpToolbarPresenter >> traverseInFocusOrderDo: aBlock excluding: excludes [

	self presentersInFocusOrder do: [ :each |
		each traverseInFocusOrderDo: aBlock excluding: excludes ]
]

{ #category : #events }
SpToolbarPresenter >> whenItemsChangeDo: aBlockClosure [ 
	
	self 
		property: #leftItems whenChangedDo: aBlockClosure;
		property: #rightItems whenChangedDo: aBlockClosure
]
