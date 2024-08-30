Class {
	#name : 'IconPacksFetchPresenter',
	#superclass : 'SpPresenter',
	#instVars : [
		'choosePresenter',
		'fetchButton',
		'buttonBar',
		'cancelButton'
	],
	#category : 'IconPacks',
	#package : 'IconPacks'
}

{ #category : 'instance creation' }
IconPacksFetchPresenter class >> open [
	<script>

	^ self new open
]

{ #category : 'layout' }
IconPacksFetchPresenter >> defaultLayout [

	^ SpBoxLayout newTopToBottom
		add: choosePresenter;
		add:  buttonBar expand: false;
		yourself
]

{ #category : 'accessing' }
IconPacksFetchPresenter >> fetchPacks [
	"Retrieve all packs available from repository"

	| jsonList |
	[
	jsonList := ZnClient new
		            get:
			            'https://api.github.com/repos/pharo-project/pharo-icon-packs/branches';
		            contents ]
		on: Error
		do: [ :e | "in case of error (no inet connection, for example) I retrieve just current icon set"
			e crTrace.
			^ { Smalltalk ui icons } ].
	^ (STON fromString: jsonList)
		  collect: [ :each |
			  | packName |
			  "Ensure we have just one instance of current pack"
			  packName := each at: 'name'.
			  packName = (self iconNamed: #name)
				  ifTrue: [ Smalltalk ui icons ]
				  ifFalse: [ ThemeIcons named: packName ] ]
		  as: Array
]

{ #category : 'initialization' }
IconPacksFetchPresenter >> fetchSelectedIconPacks [
	"Callback to make a remote request to download available icon packs"

	self selectedIconPacks 
		do: [ : iconPack | iconPack downloadFromUrl ]
		displayingProgress: 'Downloading selected icon packs'

]

{ #category : 'initialization' }
IconPacksFetchPresenter >> initializePresenters [ 

	choosePresenter := SpChooserPresenter new
		sourceItems: self fetchPacks;
		displayBlock: [ : themeIcons | themeIcons name ];
		yourself.

	fetchButton := self newButton
		help: 'Fetch Icon Packs from remote repository';
		label: 'Fetch';
		icon: (self iconNamed: #down);
		action: [ self fetchSelectedIconPacks ];
		yourself.
	cancelButton := self newButton
		label: 'Close';
		icon: (self iconNamed: #smallCancel);
		action: [ self delete ];
		yourself.

	buttonBar := self newButtonBar 
		add: cancelButton;
		add:  fetchButton;
		yourself

]

{ #category : 'initialization' }
IconPacksFetchPresenter >> initializeWindow: aWindowPresenter [
	super initializeWindow: aWindowPresenter.
	
	aWindowPresenter 
		title: 'Icon Packs Downloader';
		initialExtent: (950 @ 650) scaledByDisplayScaleFactor;
		centered
]

{ #category : 'initialization' }
IconPacksFetchPresenter >> selectedIconPacks [ 
	"Answer a <Collection> of "
	
	^ choosePresenter chosenItems
]
