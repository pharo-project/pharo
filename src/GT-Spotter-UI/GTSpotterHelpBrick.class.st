"
I am brick responsible to show the help bar in the bottom of the spotter window.
"
Class {
	#name : #GTSpotterHelpBrick,
	#superclass : #GLMBrick,
	#instVars : [
		'label',
		'spotter'
	],
	#category : #'GT-Spotter-UI-Widgets'
}

{ #category : #actions }
GTSpotterHelpBrick >> anotherSuggestion [

	| suggestion |
	suggestion := {
		'Meta + n filters for Classes'.
		'Meta + m filters for Implementros'.
		'Meta + h filters for Help topics'.
		'Adding #Classes in the query filters for Classes'.
		'Adding #Implementors in the query filters for Implementros'.
		'Adding #Help in the query filters for Help Topics'.
		'Adding #Packages in the query filters for Packages'.
		'Adding #Menu in the query filters for System Menu entries'.
		'Using words separated with spaces looks up for all of them'.
		'Using camel case looks up for all of the parts'.
		'Clicking in the question mark shows another tip'.
		'Clicking in this tip shows another tip'.
	} atRandom.
	
	[label text = suggestion] 
		whileTrue: [ ^ self anotherSuggestion ].
	
	label text: suggestion.
]

{ #category : #initialization }
GTSpotterHelpBrick >> color: aColor [

	super color: aColor.
	label color: aColor.
]

{ #category : #initialization }
GTSpotterHelpBrick >> initialize [
	| btn advanceSearch |
	super initialize.

	self
		hSpaceFill;
		vSpaceFill.

	label := self newSuggestionLabel.
	btn := self newSuggestionButton.
	advanceSearch := self newAdvancedSearchLink.

	self addBrickBack: btn.
	self addBrickBack: label.
	self addBrickBack: advanceSearch.
	
	self anotherSuggestion.

]

{ #category : #'initialization - widgets' }
GTSpotterHelpBrick >> newAdvancedSearchLink [
	| aBrick |
	aBrick := (GTLinkLabelMorph contents: 'Advanced Search...')
		setBalloonText: 'Perform an advanced search in the image';
		onClick: [ self openFinder ];
		asBrick.		
	
	aBrick
		width: 108;
		vSpaceFill;
		marginTop: 4.

	^ aBrick
]

{ #category : #'initialization - widgets' }
GTSpotterHelpBrick >> newSuggestionButton [
	^ GLMHintableActionButtonBrick new
		vSpaceFill;
		width: [ :m | m height ];
		margin: 0;
		hintTitle: 'Show me';
		hintText: 'another suggestion.';
		normalImage: (self theme icons iconNamed: #glamorousHelp);
		action: [ self anotherSuggestion ];
		yourself
]

{ #category : #'initialization - widgets' }
GTSpotterHelpBrick >> newSuggestionLabel [
	^ GLMLabelBrick new
		textPosition: #left;
		hSpaceFill;
		vSpaceFill;
		marginTop: 4;
		enable;
		when: #onClicked do: [ self anotherSuggestion ]
]

{ #category : #actions }
GTSpotterHelpBrick >> openFinder [

	spotter exit.
	Finder open
]

{ #category : #initialization }
GTSpotterHelpBrick >> spotter: anObject [

	spotter := anObject
]
