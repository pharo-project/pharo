"
I am a widget for the ̀Finder̀ tool. I represent contain the buttons used for
interacting with the results of a search.
"
Class {
	#name : #FinderResultButtonBar,
	#superclass : #SpPresenter,
	#instVars : [
		'browseButton',
		'sendersButton',
		'versionsButton',
		'inheritanceButton',
		'hierarchyButton',
		'implementersButton'
	],
	#category : #'Tool-Finder-Wigdets'
}

{ #category : #api }
FinderResultButtonBar >> browseAction: aBlock [

	self browseButton action: aBlock
]

{ #category : #accessing }
FinderResultButtonBar >> browseButton [

	^ browseButton
]

{ #category : #layout }
FinderResultButtonBar >> defaultLayout [

	^ 	SpBoxLayout newHorizontal
		spacing: 2;
		add: browseButton expand: false;
		add: sendersButton expand: false;
		add: implementersButton expand: false;
		add: versionsButton expand: false;
		add: inheritanceButton expand: false;
		add: hierarchyButton expand: false;
		yourself
]

{ #category : #enabling }
FinderResultButtonBar >> disableAll [

	browseButton disable.
	sendersButton disable.
	implementersButton disable.
	versionsButton disable.
	inheritanceButton disable.
	hierarchyButton disable
]

{ #category : #enabling }
FinderResultButtonBar >> enableButtonsFor: aFinderResult [
	"Enables or disables result buttons depending on the defined
	 actions for aFinderResult."

	browseButton enabled: aFinderResult hasBrowseAction.
	sendersButton enabled: aFinderResult hasSendersAction.
	implementersButton enabled: aFinderResult hasImplementersAction.
	versionsButton enabled: aFinderResult hasVersionsAction.
	inheritanceButton enabled: aFinderResult hasInheritanceAction.
	hierarchyButton enabled: aFinderResult hasHierarchyAction
]

{ #category : #api }
FinderResultButtonBar >> hierarchyAction: aBlock [

	self hierarchyButton action: aBlock
]

{ #category : #accessing }
FinderResultButtonBar >> hierarchyButton [

	^ hierarchyButton
]

{ #category : #api }
FinderResultButtonBar >> implementersAction: aBlock [

	self implementersButton action: aBlock
]

{ #category : #accessing }
FinderResultButtonBar >> implementersButton [

	^ implementersButton
]

{ #category : #api }
FinderResultButtonBar >> inheritanceAction: aBlock [

	self inheritanceButton action: aBlock
]

{ #category : #accessing }
FinderResultButtonBar >> inheritanceButton [

	^ inheritanceButton
]

{ #category : #initialization }
FinderResultButtonBar >> initializePresenters [

	browseButton := self newButton label: 'Browse'.
	sendersButton := self newButton label: 'Senders'.
	implementersButton := self newButton label: 'Implementors'.
	versionsButton := self newButton label: 'Versions'.
	inheritanceButton := self newButton label: 'Inheritance'.
	hierarchyButton := self newButton label: 'Hierarchy'.

	self disableAll
]

{ #category : #api }
FinderResultButtonBar >> sendersAction: aBlock [

	self sendersButton action: aBlock
]

{ #category : #accessing }
FinderResultButtonBar >> sendersButton [

	^ sendersButton
]

{ #category : #api }
FinderResultButtonBar >> versionsAction: aBlock [

	self versionsButton action: aBlock
]

{ #category : #accessing }
FinderResultButtonBar >> versionsButton [

	^ versionsButton
]
