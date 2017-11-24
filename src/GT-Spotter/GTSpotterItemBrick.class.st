"
I am a visual element that represents a single search result item within a category and is located in the list of all result items in Spotter.

I consist of icon, label and action bar with item related actions. Generally, I look like:

[ .... == Icon ==  .........    ==Label==   ........................ == Action bar == ..... ] >

I can be hovered with the mouse and selected by navigating with the help of keyboard arrows or click-selected with mouse.
Once hovered or selected my action buttons become visible.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	actionbarBrick:		<GLMBrick> - is a container of action buttons
	iconBrick:		<GLMBrick> - represents a search result icon. May be empty / invisible, still occupies space
	labelBrick:		<GLMBrick> - shows a textual representation of a search result item
	candidateLink:		<GTSpotterCandidateLink> - a link to the search result candidate. Link know its successor and predecessor within a result list
	stepModel:		<GTSpotterStep> - reference to the spotter step context that produced a candidate item I visually represent


    Implementation Points
"
Class {
	#name : #GTSpotterItemBrick,
	#superclass : #GLMInteractableBrick,
	#instVars : [
		'iconBrick',
		'labelBrick',
		'actionbarBrick',
		'stepModel',
		'candidateLink'
	],
	#category : #'GT-Spotter-UI'
}

{ #category : #accessing }
GTSpotterItemBrick >> actionbarBrick [
	"Return action bar brick"

	^ actionbarBrick
]

{ #category : #accessing }
GTSpotterItemBrick >> actionbarBrick: aBrick [

	actionbarBrick ifNotNil: [ self removeBrick: actionbarBrick ].
	actionbarBrick := aBrick.

	
	self addBrickBack: actionbarBrick
]

{ #category : #actions }
GTSpotterItemBrick >> beSelected [
	self stepModel select: self candidateLink
]

{ #category : #accessing }
GTSpotterItemBrick >> candidateLink [

	^ candidateLink
]

{ #category : #accessing }
GTSpotterItemBrick >> candidateLink: aDoubleLink [ 

	candidateLink := aDoubleLink.
	
	self actionbarBrick candidateLink: self candidateLink
]

{ #category : #'event handling' }
GTSpotterItemBrick >> check [
	"#check on UI side means that item is selected in Spotter model"
	super check.

	self showActionbar.
	self notifySelected
]

{ #category : #'event handling' }
GTSpotterItemBrick >> checkOrUncheck [

	(self stepModel isSelected: self candidateLink)
		ifTrue: [ self isChecked ifFalse: [ self check ] ]
		ifFalse: [ self isChecked ifTrue: [ self uncheck ] ]
]

{ #category : #'event handling' }
GTSpotterItemBrick >> deselect [
	super deselect.
	self hideActionbar
]

{ #category : #'morphic - events' }
GTSpotterItemBrick >> doubleClick: anEvent [

	self stepModel actDueTo: #doubleClick
]

{ #category : #'morphic - events' }
GTSpotterItemBrick >> handlesMouseDown: evt [
	^ true
]

{ #category : #actions }
GTSpotterItemBrick >> hideActionbar [

	self shouldShowActionbar
		ifTrue: [ ^ self ].

	self actionbarBrick visible: false
]

{ #category : #actions }
GTSpotterItemBrick >> highlightQuery [
	self labelBrick label highlight: self stepModel context text
]

{ #category : #accessing }
GTSpotterItemBrick >> icon: aForm [

	self iconBrick normalImage: aForm.
]

{ #category : #accessing }
GTSpotterItemBrick >> iconBrick [
	^ iconBrick
]

{ #category : #accessing }
GTSpotterItemBrick >> iconBrick: aBrick [

	iconBrick ifNotNil: [ self removeBrick: iconBrick ].
	iconBrick := aBrick.

	self themer spotterThemer itemIconStyleFor: iconBrick.

	self addBrickBack: iconBrick
]

{ #category : #initialization }
GTSpotterItemBrick >> initialize [
	super initialize.
	self enable.

	self themer spotterThemer itemNormalStyleFor: self.

	self iconBrick: self newIconBrick.
	self labelBrick: self newLabelBrick.
	self actionbarBrick: self newActionbarBrick
]

{ #category : #accessing }
GTSpotterItemBrick >> labelBrick [
	^ labelBrick
]

{ #category : #accessing }
GTSpotterItemBrick >> labelBrick: aBrick [

	labelBrick ifNotNil: [self removeBrick: labelBrick].
	labelBrick := aBrick.
	
	self themer spotterThemer itemLabelStyleFor: labelBrick.
	
	self addBrickBack: labelBrick
]

{ #category : #'morphic - events' }
GTSpotterItemBrick >> mouseDown: evt [
	|selectors|
	
	self isEnabled ifFalse: [ ^ self ].
	
	selectors := Array
				with: #singleClick:
				with: #doubleClick:
				with: nil
				with: nil.
				
	evt hand
		waitForClicksOrDrag: self
		event: evt
		selectors: selectors
		threshold: 10
	
]

{ #category : #'instance creation' }
GTSpotterItemBrick >> newActionbarBrick [

	^ GTSpotterItemActionbarBrick new
]

{ #category : #'instance creation' }
GTSpotterItemBrick >> newIconBrick [

	^ GLMInteractableBrick new normalImage: ((Form extent: 16@16 depth: 32) fillAlpha: 0)
]

{ #category : #'instance creation' }
GTSpotterItemBrick >> newLabelBrick [
	self flag: 'find a better number, or at least a better place for the number :)'.

	^ GLMLabelBrick new 
		text: 'Search result with very very long name';
		paddingRight: 16
]

{ #category : #notifying }
GTSpotterItemBrick >> notifySelected [

	self stepModel spotter announce: (GTSpotterCandidateSelected new itemBrick: self).
	self stepModel announcer announce: (GTSpotterCandidateSelected new itemBrick: self)
]

{ #category : #'brick-layout-events' }
GTSpotterItemBrick >> onLayouted [
	super onLayouted.
	
	self isChecked ifTrue: [ self stepModel spotter announce: GTSpotterCandidateSelectedMoved new ]
]

{ #category : #'brick-structure-events' }
GTSpotterItemBrick >> onRemovedFrom: aBrick [
	super onRemovedFrom: aBrick.
	
	"I must not receive any announcements as soon as i'm removed from dom"
	self stepModel ifNotNil: [
		self stepModel announcer unsubscribe: self.
		self stepModel spotter announcer unsubscribe: self  ]
]

{ #category : #'event handling' }
GTSpotterItemBrick >> onSelectedChanged: anAnnouncement [

	self checkOrUncheck
]

{ #category : #'event handling' }
GTSpotterItemBrick >> onStepPopped [

	
	(self stepModel spotter steps last = self stepModel) & self isChecked ifTrue: [
		self notifySelected ]
]

{ #category : #'brick-properties-events' }
GTSpotterItemBrick >> onThemerChanged [
	super onThemerChanged.
	
	self themer spotterThemer itemNormalWidgetStyleFor: self
]

{ #category : #accessing }
GTSpotterItemBrick >> processor [
	^ self candidateLink processor
]

{ #category : #'event handling' }
GTSpotterItemBrick >> select [
	super select.
	self showActionbar
]

{ #category : #testing }
GTSpotterItemBrick >> shouldShowActionbar [
	"Return true if actionbar should be shown, false otherwise"
	<return: #Boolean>

	^ self isHovered or: [
		self isChecked or: [
			self stepModel isSelected: self candidateLink ] ]
]

{ #category : #actions }
GTSpotterItemBrick >> showActionbar [
	self shouldShowActionbar
		ifFalse: [ ^ self ].

	self actionbarBrick visible: true
]

{ #category : #'morphic - events' }
GTSpotterItemBrick >> singleClick: evt [

	self stepModel select: self candidateLink dueTo: #singleClick
]

{ #category : #accessing }
GTSpotterItemBrick >> stepModel [

	^ stepModel
]

{ #category : #accessing }
GTSpotterItemBrick >> stepModel: aModel [

	stepModel := aModel.
	
	stepModel announcer when: GTSpotterSelectedCandidateChanged send: #onSelectedChanged: to: self.
	stepModel spotter announcer when: GTSpotterStepPopped send: #onStepPopped to: self.
	
	self actionbarBrick stepModel: aModel.
	
	self
		highlightQuery;
		checkOrUncheck
]

{ #category : #accessing }
GTSpotterItemBrick >> text: aString [

	self labelBrick text: aString
]

{ #category : #'event handling' }
GTSpotterItemBrick >> uncheck [
	super uncheck.
	self hideActionbar
]
