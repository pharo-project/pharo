"
I am a presenter for a contact book.

I provide a nice UI to manage the contacts of the contact book I am opened on.

Check my class-side for example.
"
Class {
	#name : #SpContactBookPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'table',
		'contactBook',
		'menuBar'
	],
	#category : #'Spec2-Commander2-ContactBook-Spec'
}

{ #category : #commands }
SpContactBookPresenter class >> buildAddingGroupWith: presenterIntance [
	^ (CmCommandGroup named: 'Adding') asSpecGroup
		description: 'Commands related to contact addition.';
		register: SpAddContactCommand forSpec;
		beDisplayedAsGroup;
		yourself
]

{ #category : #commands }
SpContactBookPresenter class >> buildCommandsGroupWith: presenterIntance forRoot: rootCommandGroup [
	rootCommandGroup
		register: (self buildMenuBarGroupWith: presenterIntance);
		register: (self buildContextualMenuGroupWith: presenterIntance)
]

{ #category : #commands }
SpContactBookPresenter class >> buildContextualMenuGroupWith: presenterIntance [
	^ (CmCommandGroup named: 'Context Menu') asSpecGroup
		register: (self buildEditionGroupWith: presenterIntance);
		register: (self buildAddingGroupWith: presenterIntance);
		register: (self buildRemovingGroupWith: presenterIntance);
		yourself
]

{ #category : #commands }
SpContactBookPresenter class >> buildEditionGroupWith: presenterIntance [
	^ (CmCommandGroup named: 'Edition') asSpecGroup
		description: 'Commands related to contact edition.';
		register: SpRenameContactCommand forSpec;
		beDisplayedAsGroup;
		yourself
]

{ #category : #commands }
SpContactBookPresenter class >> buildMenuBarGroupWith: presenterIntance [
	^ (CmCommandGroup named: 'MenuBar') asSpecGroup
		register: SpPrintContactBookInTranscript forSpec;
		yourself
]

{ #category : #commands }
SpContactBookPresenter class >> buildRemovingGroupWith: presenterIntance [
	^ (CmCommandGroup named: 'Removing') asSpecGroup
		description: 'Command related to contact removal.';
		register: SpRemoveContactCommand forSpec;
		beDisplayedAsGroup;
		yourself
]

{ #category : #examples }
SpContactBookPresenter class >> coworkersExample [
	<example>
	^ (self on: SpContactBook coworkers) openWithSpec 
]

{ #category : #examples }
SpContactBookPresenter class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: #menuBar
			withConstraints: [ :constraints | constraints height: self toolbarHeight ];
		add: #table;
		yourself
]

{ #category : #examples }
SpContactBookPresenter class >> familyExample [
	<example>
	^ (self on: SpContactBook family) openWithSpec 
]

{ #category : #accessing }
SpContactBookPresenter >> contactBook [
	^ contactBook
]

{ #category : #initialization }
SpContactBookPresenter >> initializePresenters [
	table := self newTable.
	table 
		addColumn: (SpStringTableColumn title: 'Name' evaluated: #name);
		addColumn: (SpStringTableColumn title: 'Phone' evaluated: #phone).
	
	table contextMenuFromCommandsGroup: [ self rootCommandsGroup / 'Context Menu' ].
	
	table items: contactBook contents.
	
	menuBar := (self rootCommandsGroup / 'MenuBar') asMenuBarPresenter.
]

{ #category : #initialization }
SpContactBookPresenter >> initializeWindow: aWindowPresenter [
	
	super initializeWindow: aWindowPresenter.
	self rootCommandsGroup installShortcutsIn: aWindowPresenter
]

{ #category : #testing }
SpContactBookPresenter >> isContactSelected [
	^ table selection selectedItem notNil
]

{ #category : #'user interaction' }
SpContactBookPresenter >> newContact [
	| rawData splitted |
	rawData := self request: 'Enter new contact name and phone (split by comma)' initialAnswer: '' title: 'Create new contact'.
	
	splitted := rawData splitOn: $,.
	
	(splitted size = 2 and: [ splitted allSatisfy: #isNotEmpty ])
		ifFalse: [ SpInvalidUserInput signal: 'Please enter contact name and phone (split by comma)'  ].
	
	^ SpContact new
		name: splitted first;
		phone: splitted second;
		yourself
]

{ #category : #'user interaction' }
SpContactBookPresenter >> newName [
	| newName |
	newName := self request: 'New name of the contact' initialAnswer: self selectedContact name title: 'Rename a contact'.
		
	(newName isEmpty)
		ifTrue: [ 
			SpInvalidUserInput signal: 'The contact name can not be empty' ].
	
	^ newName
]

{ #category : #'user interaction' }
SpContactBookPresenter >> newPhone [
	| phone |
	phone := self request: 'New phone for the contact' initialAnswer: self selectedContact phone title: 'Set new phone for contact'.
		
	(phone matchesRegex: '\d\d\d\s\d\d\d')
		ifFalse: [ 
			SpInvalidUserInput signal: 'The phone number is not well formated. Should match "\d\d\d\s\d\d\d"' ].
	
	^ phone
]

{ #category : #'user interaction' }
SpContactBookPresenter >> removeContact: aCmContact [
	| confirm |
	confirm := self confirm: ('Are you sure you want to remove {1} contact' format: { aCmContact name }) label: 'Remove the contact'.
	confirm
		ifTrue: [ self contactBook removeContact: aCmContact ]
]

{ #category : #accessing }
SpContactBookPresenter >> selectedContact [
	^ table selection selectedItem
]

{ #category : #accessing }
SpContactBookPresenter >> selectedContacts [
	^table selectedItems
]

{ #category : #initialization }
SpContactBookPresenter >> setModelBeforeInitialization: aContactBook [
	super setModelBeforeInitialization: aContactBook.
	contactBook := aContactBook
]

{ #category : #accessing }
SpContactBookPresenter >> table [
	^ table
]

{ #category : #accessing }
SpContactBookPresenter >> table: anObject [
	table := anObject
]

{ #category : #updating }
SpContactBookPresenter >> updateView [
	table items: contactBook contents
]
