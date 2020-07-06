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

{ #category : #example }
SpToolbarPresenter class >> example [
	| loremIpsumWords menuButton |
	
	loremIpsumWords := String loremIpsum substrings.
	^ SpToolbarPresenter new 
		beBoth;
		addItem: (SpToolbarButtonPresenter new 
			icon: (self iconNamed: #smallOk);
			label: 'Simple';
			action: [ UIManager default defer: [ self inform: 'Pressed' ] ];
			yourself);
		addItem: (SpToolbarToggleButtonPresenter new
			icon: (self iconNamed: #smallOk);
			label: 'Toggle';
			action: [ :value | UIManager default defer: [ self inform: value asString ] ];
			yourself);
		addItem: ((menuButton := SpToolbarMenuButtonPresenter new)
			label: 'Menu';
			icon: (self iconNamed: #smallOk);
			action: [ UIManager default defer: [ self inform: menuButton label ] ];
			menu: [ 
				SpMenuPresenter new
					addGroup: [ :group | group
						addItem: [ :item | item 
							name: '1: ', loremIpsumWords atRandom;
							action: [ menuButton label: item name ] ];
						addItem: [ :item | item 
							name: '2: ', loremIpsumWords atRandom;
							action: [ menuButton label: item name ] ];
						addItem: [ :item | item 
							name: '3: ', loremIpsumWords atRandom;
							action: [ menuButton label: item name ] ] ];
					yourself ];
			yourself);
		openWithSpec
]

{ #category : #convenience }
SpToolbarPresenter >> addButtonLabel: aLabel iconNamed: anIconName help: aHelpString action: aBlock [

	self addItem: (SpToolbarButtonPresenter new
		label: aLabel;
		icon: (self application iconNamed: anIconName);
		help: aHelpString;
		action: aBlock;
		yourself)
]

{ #category : #adding }
SpToolbarPresenter >> addItem: aToolbarItem [
	
	self addItem: aToolbarItem position: SpToolbarItemPosition left
]

{ #category : #adding }
SpToolbarPresenter >> addItem: aToolbarItem position: aPosition [
	
	aToolbarItem owner: self.
	aPosition addItem: aToolbarItem into: self
]

{ #category : #adding }
SpToolbarPresenter >> addItemLeft: aToolbarItem [ 
	
	aToolbarItem owner: self.
	leftItems add: aToolbarItem.
	self notifyPropertyChanged: #leftItems.
]

{ #category : #adding }
SpToolbarPresenter >> addItemRight: aToolbarItem [ 

	aToolbarItem owner: self.
	rightItems add: aToolbarItem.
	self notifyPropertyChanged: #rightItems.	
]

{ #category : #accessing }
SpToolbarPresenter >> beBoth [

	self displayMode: SpToolbarDisplayMode modeIconAndLabel
]

{ #category : #accessing }
SpToolbarPresenter >> beIcons [

	self displayMode: SpToolbarDisplayMode modeIcon
]

{ #category : #accessing }
SpToolbarPresenter >> beText [

	self displayMode: SpToolbarDisplayMode modeLabel
]

{ #category : #accessing }
SpToolbarPresenter >> displayMode [

	^ displayMode
]

{ #category : #accessing }
SpToolbarPresenter >> displayMode: aDisplayMode [

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

{ #category : #accessing }
SpToolbarPresenter >> itemNamed: aString [ 
	
	^ self items detect: [ :e | e label = aString ]
]

{ #category : #api }
SpToolbarPresenter >> items [

	^ leftItems , rightItems
]

{ #category : #accessing }
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

{ #category : #removing }
SpToolbarPresenter >> removeAllItems [

	leftItems := OrderedCollection new.
	rightItems := OrderedCollection new
]

{ #category : #accessing }
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
