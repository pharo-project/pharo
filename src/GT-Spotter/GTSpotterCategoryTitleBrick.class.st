Class {
	#name : #GTSpotterCategoryTitleBrick,
	#superclass : #GLMInteractableBrick,
	#instVars : [
		'nameBrick',
		'amountBrick',
		'actionbarBrick',
		'stepModel',
		'diveInButton',
		'shortcutBrick',
		'category'
	],
	#category : #'GT-Spotter-UI'
}

{ #category : #accessing }
GTSpotterCategoryTitleBrick >> actionbarBrick [
	^ actionbarBrick
]

{ #category : #accessing }
GTSpotterCategoryTitleBrick >> actionbarBrick: anObject [
	actionbarBrick := anObject.

	self actionbarBrick
		vSpaceFill;
		hShrinkWrap.
]

{ #category : #adding }
GTSpotterCategoryTitleBrick >> addDiveInButton [
	
	diveInButton := self newActionbarButton.
	self diveInButton
		icon: self themer spotterThemer iconDiveInCategory;
		pressedColor: (self themer spotterThemer itemActionSelectedBackgroundColor alpha: 0.5);
		action: [ self stepModel diveInCategory: self category dueTo: #actionButton ];
		beHelp;
		hintTitle: 'Show all';
		hintText: '<',OSPlatform current defaultModifier name asLowercase,'+shift+right>'.
		
	self pushActionButton: self diveInButton
	
]

{ #category : #accessing }
GTSpotterCategoryTitleBrick >> amountBrick [

	^ amountBrick
]

{ #category : #accessing }
GTSpotterCategoryTitleBrick >> amountBrick: aBrick [

	amountBrick ifNotNil: [self removeBrick: amountBrick].
	amountBrick := aBrick.
	
	self themer spotterThemer categoryAmountStyleFor: amountBrick.
	
	self addBrickBack: amountBrick
]

{ #category : #accessing }
GTSpotterCategoryTitleBrick >> category [
	self assert: category isNotNil.
	^ category
]

{ #category : #accessing }
GTSpotterCategoryTitleBrick >> category: aProcessor [
	self assert: aProcessor isNotNil.
	category := aProcessor
]

{ #category : #accessing }
GTSpotterCategoryTitleBrick >> diveInButton [
	^ diveInButton
]

{ #category : #actions }
GTSpotterCategoryTitleBrick >> hideActionbar [

	(self hasSubbrick: self actionbarBrick) ifTrue: [ 
		self removeBrick: self actionbarBrick ]
]

{ #category : #initialization }
GTSpotterCategoryTitleBrick >> initialize [
	super initialize.
	self enable.
	self announcer when: #onMouseHover send: #onHovered to: self.
	self announcer when: #onMouseUnhover send: #onUnhovered to: self
]

{ #category : #accessing }
GTSpotterCategoryTitleBrick >> nameBrick [

	^ nameBrick
]

{ #category : #accessing }
GTSpotterCategoryTitleBrick >> nameBrick: aBrick [

	nameBrick ifNotNil: [self removeBrick: nameBrick].
	nameBrick := aBrick.
	
	self themer spotterThemer categoryTitleNameStyleFor: nameBrick.
	
	self addBrickBack: nameBrick
]

{ #category : #'instance creation' }
GTSpotterCategoryTitleBrick >> newActionbarBrick [

	^ GLMBrick new
]

{ #category : #'instance creation' }
GTSpotterCategoryTitleBrick >> newActionbarButton [

	^ GLMHintableActionButtonBrick new
]

{ #category : #'instance creation' }
GTSpotterCategoryTitleBrick >> newAmountBrick [

	^ GLMLabelBrick new
]

{ #category : #'instance creation' }
GTSpotterCategoryTitleBrick >> newNameBrick [

	^ GLMLabelBrick new
]

{ #category : #'instance creation' }
GTSpotterCategoryTitleBrick >> newShortcutBrick [

	^ GLMLabelBrick new
]

{ #category : #'instance creation' }
GTSpotterCategoryTitleBrick >> on: aStepModel category: aProcessor [

	self stepModel: aStepModel.
	self category: aProcessor.
	
	self nameBrick: self newNameBrick.
	self nameBrick text: '#', aProcessor title.
	
	aProcessor keyBinding ifNotNil: [ 
		self shortcutBrick: self newShortcutBrick.
		self setShortcutBrickTextFor: aProcessor ].
			
	self amountBrick: self newAmountBrick.
	self amountBrick text: '0/0'.
	
	self actionbarBrick: self newActionbarBrick.
	self addDiveInButton.
	
	self onCandidateSelected.
	
	self nameBrick wrappedBounds markWidthDirty.
	self nameBrick label wrappedBounds markWidthDirty.
	self doLayoutForce.

	^ self
]

{ #category : #'event-handling' }
GTSpotterCategoryTitleBrick >> onCandidateSelected [
	
	(self stepModel isCategorySelected: self category)
		ifTrue: [ self showActionbar ]
		ifFalse: [
			"well, we need to check first if there are not hovered candidates for my category"
			(self isSelected not and: [
				self owner isNotNil and: [
					(self owner owner categories includesKey: self category) and: [ 
						(self owner owner categoryBrickOf: self category) isSelected not  ] ] ] )
			ifTrue: [ self hideActionbar ] ]
]

{ #category : #'event-handling' }
GTSpotterCategoryTitleBrick >> onHideHints [

	(self hasSubbrick: self actionbarBrick) ifFalse: [ ^ self ].
	self themer spotterThemer hintHideStyleFor: self diveInButton
]

{ #category : #'event-handling' }
GTSpotterCategoryTitleBrick >> onHovered [
	self showActionbar
]

{ #category : #layouting }
GTSpotterCategoryTitleBrick >> onLayoutEvent: aLayoutEvent [
	super onLayoutEvent: aLayoutEvent.
	
	(aLayoutEvent isKindOf: GLMBrickLayoutStacked) ifTrue: [ "self enable" ]
]

{ #category : #'event-handling' }
GTSpotterCategoryTitleBrick >> onRevealHints [

	(self hasSubbrick: self actionbarBrick) ifFalse: [ ^ self ].
	self themer spotterThemer hintRevealStyleFor: self diveInButton
]

{ #category : #'brick-properties-events' }
GTSpotterCategoryTitleBrick >> onThemerChanged [
	super onThemerChanged.
	
	self themer spotterThemer categoryTitleWidgetStyleFor: self.
	self themer spotterThemer categoryAmountWidgetStyleFor: self amountBrick.
	self themer spotterThemer categoryTitleNameWidgetStyleFor: self nameBrick.
]

{ #category : #'event-handling' }
GTSpotterCategoryTitleBrick >> onUnhovered [
	(self stepModel isCategorySelected: self category)
		ifFalse: [ self hideActionbar ]
]

{ #category : #adding }
GTSpotterCategoryTitleBrick >> pushActionButton: aButton [

	aButton
		width: [ :b | b icon width * 1.5 ];
		vSpaceFill.
		
	self actionbarBrick addBrickBack: aButton.
	
	self stepModel spotter isShiftPressed
		ifTrue: [ self themer spotterThemer hintRevealStyleFor: aButton ]
]

{ #category : #accessing }
GTSpotterCategoryTitleBrick >> setAmountVisible: visible all: all [

	self amountBrick text: (visible asString, '/', all asString).
	self amountBrick wrappedBounds markWidthDirty.
	self amountBrick doLayout.
	self doLayoutForce
]

{ #category : #'instance creation' }
GTSpotterCategoryTitleBrick >> setShortcutBrickTextFor: aProcessor [
	
	self shortcutBrick text: (ToggleMenuItemShortcut 
		owner: nil 
		keyText: 
			(aProcessor keyBinding printString 
				copyReplaceAll: 'Meta' 
				with: OSPlatform current defaultModifier name)) text
]

{ #category : #accessing }
GTSpotterCategoryTitleBrick >> shortcutBrick [

	^ shortcutBrick
]

{ #category : #accessing }
GTSpotterCategoryTitleBrick >> shortcutBrick: aBrick [

	shortcutBrick ifNotNil: [self removeBrick: shortcutBrick].
	shortcutBrick := aBrick.
	
	self themer spotterThemer categoryTitleShortcutStyleFor: shortcutBrick.
	
	self addBrickBack: shortcutBrick
]

{ #category : #actions }
GTSpotterCategoryTitleBrick >> showActionbar [

	(self hasSubbrick: self actionbarBrick) ifFalse: [ 
		self addBrickBack: self actionbarBrick.
		self toggleHints ]
]

{ #category : #accessing }
GTSpotterCategoryTitleBrick >> stepModel [

	^ stepModel
]

{ #category : #accessing }
GTSpotterCategoryTitleBrick >> stepModel: anObject [
	stepModel := anObject.
	
	self stepModel announcer when: GTSpotterSelectedCandidateChanged send: #onCandidateSelected to: self.
	self stepModel spotter announcer when: GTSpotterRevealHints send: #onRevealHints to: self.
	self stepModel spotter announcer when: GTSpotterHideHints send: #onHideHints to: self
]

{ #category : #actions }
GTSpotterCategoryTitleBrick >> toggleHints [

	(self hasSubbrick: self actionbarBrick) ifFalse: [ ^ self ].
	self stepModel spotter isShiftPressed
		ifTrue: [ self onRevealHints ]
		ifFalse: [ self onHideHints ]
]
