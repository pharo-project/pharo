"
I am a Spec2 dialog for entering new project details to add to the Quick Project Loader.

Inputs:
- Project name (required)
- Repository URL (required, e.g., 'github://owner/repo/src')
- Baseline name (optional, e.g. 'MyProject' without BaselineOf)
- Groups (optional, comma-separated)
- Use incoming on conflict checkbox (default: checked)

On accept (main dialog OK button), I answer a new QuickProjectEntry.
"
Class {
	#name : 'QuickProjectAddPresenter',
	#superclass : 'SpPresenter',
	#instVars : [
		'nameInput',
		'urlInput',
		'baselineInput',
		'groupsInput',
		'useIncomingCheckBox'
	],
	#category : 'QuickProjectLoader-UI',
	#package : 'QuickProjectLoader',
	#tag : 'UI'
}

{ #category : 'accessing' }
QuickProjectAddPresenter class >> defaultPreferredExtent [

	^ 400 @ 320
]

{ #category : 'instance creation' }
QuickProjectAddPresenter class >> open [

	^ (self newApplication: StPharoApplication current) open
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
		isManual: true;
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
		yourself
]

{ #category : 'accessing' }
QuickProjectAddPresenter >> groupsInput [

	^ groupsInput
]

{ #category : 'initialization' }
QuickProjectAddPresenter >> initializeDialogWindow: aDialogPresenter [

	aDialogPresenter
		addButton: 'Cancel' do: [ :dialog | dialog close ];
		addButton: 'OK' do: [ :dialog | 
			self isFormValid ifFalse: [ 
				self inform: 'Please fill in required fields (Name and Repository URL)'.
				^ self ].
			dialog beOk.
			dialog close ]
]

{ #category : 'initialization' }
QuickProjectAddPresenter >> initializePresenters [

	nameInput := self newTextInput
		placeholder: 'My Project';
		yourself.
	urlInput := self newTextInput
		placeholder: 'github://owner/repo/src';
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

{ #category : 'private' }
QuickProjectAddPresenter >> parseGroups: aString [

	| trimmed |
	trimmed := aString trimmed.
	trimmed ifEmpty: [ ^ #() ].
	^ (trimmed substrings: $,) collect: [ :each | each trimmed ]
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
