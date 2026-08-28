"
I am a Spec2 dialog for entering new project details to add to the Quick Project Loader.

I provide text inputs for:
- Project name (required)
- Repository URL (required, e.g., 'github://owner/repo/src')
- Baseline name (required, without 'BaselineOf' prefix)
- Groups (optional, comma-separated)

On accept, I answer a new QuickProjectEntry. On cancel, I answer nil.

## Usage

```
QuickProjectAddPresenter new open.
```
"
Class {
	#name : 'QuickProjectAddPresenter',
	#superclass : 'SpPresenter',
	#instVars : [
		'nameInput',
		'urlInput',
		'baselineInput',
		'groupsInput',
		'acceptButton',
		'cancelButton'
	],
	#category : 'QuickProjectLoader-UI',
	#package : 'QuickProjectLoader',
	#tag : 'UI'
}

{ #category : 'accessing' }
QuickProjectAddPresenter class >> defaultPreferredExtent [

	^ 500 @ 350
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
	"Create and answer a new QuickProjectEntry from the current form values."

	| parsedGroups |
	parsedGroups := self parseGroups: groupsInput text.
	^ QuickProjectEntry
		name: nameInput text trimmed
		repositoryUrl: urlInput text trimmed
		baseline: baselineInput text trimmed
		groups: parsedGroups
]

{ #category : 'layout' }
QuickProjectAddPresenter >> defaultLayout [

	^ SpBoxLayout newTopToBottom
		spacing: 5;
		add: 'Project Name:' expand: false;
		add: nameInput expand: false;
		add: 'Repository URL (e.g., github://owner/repo/src):' expand: false;
		add: urlInput expand: false;
		add: 'Baseline Name (without BaselineOf prefix):' expand: false;
		add: baselineInput expand: false;
		add: 'Groups (comma-separated, leave empty for default):' expand: false;
		add: groupsInput expand: false;
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
		placeholder: 'MyProject';
		whenTextChangedDo: [ :text | self updateAcceptButton ];
		yourself.
	groupsInput := self newTextInput
		placeholder: 'default, tests (optional)';
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
	"Answer true if all required fields have non-empty values."

	^ nameInput text trimmed isNotEmpty
		and: [ urlInput text trimmed isNotEmpty
		and: [ baselineInput text trimmed isNotEmpty ] ]
]

{ #category : 'accessing' }
QuickProjectAddPresenter >> nameInput [

	^ nameInput
]

{ #category : 'actions' }
QuickProjectAddPresenter >> onAccept [
	"Validate form, create the entry, and close the dialog."

	self isFormValid ifFalse: [ 
		self inform: 'Please fill in all required fields'.
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
	"Parse a comma-separated groups string into an array of trimmed strings.
	Answer an empty array if the input is blank."

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

{ #category : 'initialization' }
QuickProjectAddPresenter >> windowTitle [

	^ 'Add Project'
]
