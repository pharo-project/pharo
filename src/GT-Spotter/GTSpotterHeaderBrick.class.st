Class {
	#name : #GTSpotterHeaderBrick,
	#superclass : #GLMBrick,
	#instVars : [
		'stepsBrick',
		'searchField',
		'closeButton',
		'spotterModel',
		'settingsButton',
		'helpButton'
	],
	#category : #'GT-Spotter-UI'
}

{ #category : #accessing }
GTSpotterHeaderBrick >> closeButton [

	^ closeButton
]

{ #category : #accessing }
GTSpotterHeaderBrick >> closeButton: aBrick [

	closeButton ifNotNil: [self removeBrick: closeButton].
	closeButton := aBrick.
	
	self themer spotterThemer closeButtonWidgetStyleFor: closeButton.
	self themer spotterThemer closeButtonStyleFor: closeButton.
	
	closeButton
		hintTitle: 'Close Spotter';
		hintText: '<escape>';
		action: [ self spotterModel exitDueTo: #actionButton ].
	
	self addBrickBack: closeButton
]

{ #category : #accessing }
GTSpotterHeaderBrick >> helpButton [

	^ helpButton
]

{ #category : #accessing }
GTSpotterHeaderBrick >> helpButton: aBrick [

	| tree item |
	helpButton ifNotNil: [self removeBrick: helpButton].
	helpButton := aBrick.
	
	self themer spotterThemer helpButtonWidgetStyleFor: helpButton.
	self themer spotterThemer helpButtonStyleFor: helpButton.
	
	helpButton
		hintTitle: 'Spotter help';
		action: [ 
			self spotterModel exitDueTo: #actionButton.
			tree := HelpBrowser open instVarNamed: #treeMorph.
			item := tree allItems detect: [:each | 
				each complexContents item key = GTSpotterHelp key].
			tree selection: item ].
	
	self addBrickBack: helpButton
]

{ #category : #initialization }
GTSpotterHeaderBrick >> initialize [
	super initialize.
	
	self searchField: self newSearchField.
	self class environment at: #GTEventRecorderSettings ifPresent: [
		self settingsButton: self newSettingsButton ].
	self helpButton: self newHelpButton.
	self closeButton: self newCloseButton.
	
]

{ #category : #'instance creation' }
GTSpotterHeaderBrick >> newCloseButton [

	^ GLMHintableActionButtonBrick new
]

{ #category : #'instance creation' }
GTSpotterHeaderBrick >> newHelpButton [

	^ GLMHintableActionButtonBrick new
]

{ #category : #'instance creation' }
GTSpotterHeaderBrick >> newSearchField [

	^ GLMRubTextFieldBrick new
]

{ #category : #'instance creation' }
GTSpotterHeaderBrick >> newSettingsButton [

	^ GLMHintableActionButtonBrick new
]

{ #category : #'instance creation' }
GTSpotterHeaderBrick >> newStepsBand [

	^ GLMBrick new
]

{ #category : #'event handling' }
GTSpotterHeaderBrick >> onCurrentStepChanged [

	self searchField rubric updateTextWith: self spotterModel currentStep context text.
	self searchField rubric selectFrom: self searchField rubric text size + 1 to: self searchField rubric text size
]

{ #category : #'event handling' }
GTSpotterHeaderBrick >> onHideHints [

	self themer spotterThemer hintHideStyleFor: self settingsButton.
	self themer spotterThemer hintHideStyleFor: self closeButton.
	self themer spotterThemer hintHideStyleFor: self helpButton.
]

{ #category : #'event handling' }
GTSpotterHeaderBrick >> onRevealHints [

	self themer spotterThemer hintRevealStyleFor: self settingsButton.
	self themer spotterThemer hintRevealStyleFor: self closeButton.
	self themer spotterThemer hintRevealStyleFor: self helpButton
]

{ #category : #'brick-properties-events' }
GTSpotterHeaderBrick >> onThemerChanged [
	super onThemerChanged.
	
	self themer spotterThemer headerWidgetStyleFor: self.
	self themer spotterThemer closeButtonWidgetStyleFor: self closeButton.
	self themer spotterThemer searchFieldWidgetStyleFor: self searchField.
	self themer spotterThemer settingsButtonWidgetStyleFor: self settingsButton
]

{ #category : #accessing }
GTSpotterHeaderBrick >> openSettingsBrowser [
	GLMPrivacyAgreementBrick openSettingsBrowser.
	self spotterModel exitDueTo: #openSettingsBrowser.
]

{ #category : #accessing }
GTSpotterHeaderBrick >> searchField [

	^ searchField
]

{ #category : #accessing }
GTSpotterHeaderBrick >> searchField: aBrick [

	searchField := aBrick.
	
	self themer spotterThemer searchFieldStyleFor: searchField.
	
	searchField morph takeKeyboardFocus.
	
	self addBrickBack: searchField
]

{ #category : #accessing }
GTSpotterHeaderBrick >> settingsButton [

	^ settingsButton
]

{ #category : #accessing }
GTSpotterHeaderBrick >> settingsButton: aBrick [

	settingsButton ifNotNil: [self removeBrick: settingsButton].
	settingsButton := aBrick.
	
	self themer spotterThemer settingsButtonWidgetStyleFor: settingsButton.
	self themer spotterThemer settingsButtonStyleFor: settingsButton.
	
	settingsButton
		hintTitle: 'Settings';
		hintText: 'Shows the settings UI';
		action: [ self openSettingsBrowser ].
	
	self addBrickBack: settingsButton
]

{ #category : #accessing }
GTSpotterHeaderBrick >> spotterModel [

	^ spotterModel
]

{ #category : #accessing }
GTSpotterHeaderBrick >> spotterModel: aModel [ 

	spotterModel := aModel.
	self searchField morph on: spotterModel.

	spotterModel announcer when: GTSpotterCurrentStepChanged send: #onCurrentStepChanged to: self.
	
	self spotterModel announcer when: GTSpotterRevealHints send: #onRevealHints to: self.
	self spotterModel announcer when: GTSpotterHideHints send: #onHideHints to: self.
]
