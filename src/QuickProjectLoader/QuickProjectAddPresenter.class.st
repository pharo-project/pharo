"
I am a Spec2 dialog for entering new project details to add to the Quick Project Loader.

Inputs:
- Project name (required)
- Repository URL (required, e.g., 'github://owner/repo/src')
- Baseline name (optional, e.g. 'MyProject' without BaselineOf)
- Groups (optional, comma-separated)
- Use incoming on conflict checkbox (default: checked)

On accept, I answer a new QuickProjectEntry.
"
Class {
	#name : 'QuickProjectAddPresenter',
	#superclass : 'SpPresenter',
	#instVars : [
		'nameInput',
		'urlInput',
		'baselineInput',
		'groupsInput',
		'useIncomingCheckBox',
		'acceptButton',
		'cancelButton'
	],
	#category : 'QuickProjectLoader-UI',
	#package : 'QuickProjectLoader',
	#tag : 'UI'
}

{ #category : 'accessing' }
QuickProjectAddPresenter class >> defaultPreferredExtent [

	^ 400 @ 380
]

{ #category : 'instance creation' }
QuickProjectAddPresenter class >> open [

	^ (self newApplication: StPharoApplication current) open
]

{ #category : 'accessing' }
QuickProjectAddPresenter >> acceptButton [

	^ acceptButton
]

{ #category : 'accessing' }
QuickProjectAddPresenter >> baselineInput [

	^ baselineInput
]

{ #category : 'actions' }
QuickProjectAddPresenter >> createEntry [

	| parsedGroups |
	parsedGroups := self parseGroups: groupsInput text.
	^ QuickProjectEntry new
		name: nameInput text trimmed;
		repositoryUrl: urlInput text trimmed;
		baselineName: baselineInput text trimmed;
		groups: parsedGroups;
		useIncomingOnConflict: useIncomingCheckBox state;
		yourself
]

{ #category : 'layout' }
QuickProjectAddPresenter >> defaultLayout [

	^ SpBoxLayout newTopToBottom
		spacing: 5;
		add: 'Project Name (required):' expand: false;
		add: nameInput expand: false;
		add: 'Repository URL (required):' expand: false;
		add: urlInput expand: false;
		add: 'Baseline Name (optional):' expand: false;
		add: baselineInput expand: false;
		add: 'Groups (comma-separated, optional):' expand: false;
		add: groupsInput expand: false;
		add: useIncomingCheckBox expand: false;
		add: (SpBoxLayout newLeftToRight
			addLast: cancelButton;
			addLast: acceptButton;
			yourself) expand: false;
		yourself
]

{ #category : 'accessing' }
QuickProjectAddPresenter >> groupsInput [

	^ groupsInput
]

{ #category : 'initialization' }
QuickProjectAddPresenter >> initializePresenters [

	nameInput := self newTextInput
		placeholder: 'My Project';
		whenTextChangedDo: [ :text | self updateAcceptButton ];
		yourself.
	urlInput := self newTextInput
		placeholder: 'github://owner/repo/src';
		whenTextChangedDo: [ :text | self updateAcceptButton ];
		yourself.
	baselineInput := self newTextInput
		placeholder: 'MyProject (optional)';
		yourself.
	groupsInput := self newTextInput
		placeholder: 'default, tests (optional)';
		yourself.
	useIncomingCheckBox := self newCheckBox
		label: 'Use incoming on conflict (onConflictUseIncoming)';
		state: true;
		yourself.
	acceptButton := self newButton
		label: 'OK';
		icon: (self iconNamed: #smallOk);
		action: [ self onAccept ];
		disable;
		yourself.
	cancelButton := self newButton
		label: 'Cancel';
		icon: (self iconNamed: #smallCancel);
		action: [ self onCancel ];
		yourself
]

{ #category : 'testing' }
QuickProjectAddPresenter >> isFormValid [

	^ nameInput text trimmed isNotEmpty
		and: [ urlInput text trimmed isNotEmpty ]
]

{ #category : 'accessing' }
QuickProjectAddPresenter >> nameInput [

	^ nameInput
]

{ #category : 'actions' }
QuickProjectAddPresenter >> onAccept [

	self isFormValid ifFalse: [ 
		self inform: 'Please fill in required fields (Name and Repository URL)'.
		^ self ].
	self owner 
		ifNotNil: [ :o | 
			o close.
			^ self createEntry ]
		ifNil: [ 
			self delete.
			^ self createEntry ]
]

{ #category : 'actions' }
QuickProjectAddPresenter >> onCancel [

	self owner
		ifNotNil: [ :o | o close ]
		ifNil: [ self delete ]
]

{ #category : 'private' }
QuickProjectAddPresenter >> parseGroups: aString [

	| trimmed |
	trimmed := aString trimmed.
	trimmed ifEmpty: [ ^ #() ].
	^ (trimmed substrings: $,) collect: [ :each | each trimmed ]
]

{ #category : 'private' }
QuickProjectAddPresenter >> updateAcceptButton [

	self isFormValid
		ifTrue: [ acceptButton enable ]
		ifFalse: [ acceptButton disable ]
]

{ #category : 'accessing' }
QuickProjectAddPresenter >> urlInput [

	^ urlInput
]

{ #category : 'accessing' }
QuickProjectAddPresenter >> useIncomingCheckBox [

	^ useIncomingCheckBox
]

{ #category : 'initialization' }
QuickProjectAddPresenter >> windowTitle [

	^ 'Add Project'
]
