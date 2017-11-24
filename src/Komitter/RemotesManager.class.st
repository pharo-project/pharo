"
I am an user interface used to link mcpackages and remotes.

RemotesManager new openWithSpec
"
Class {
	#name : #RemotesManager,
	#superclass : #ComposablePresenter,
	#instVars : [
		'remotes',
		'addRemote',
		'removeRemote',
		'editRemote',
		'selectedChangedBlock',
		'selectedRemotes'
	],
	#classInstVars : [
		'addRemoteIcon',
		'removeRemoteIcon',
		'editRemoteIcon'
	],
	#category : #'Komitter-Support'
}

{ #category : #specs }
RemotesManager class >> defaultSpec [
	<spec: #default>
	^ SpecLayout composed
		newColumn: [ :col | 
			col
				newRow: [ :r | 
					r
						newColumn: [ :c |  ];
						add: #addRemoteWidget width: self buttonHeight + 4;
						add: #removeRemoteWidget width: self buttonHeight + 4;
						add: #editRemoteWidget  width: self buttonHeight ]
					height: self buttonHeight;
					add: #remotes ];
		yourself
]

{ #category : #private }
RemotesManager >> addRemote [
	
	MCTool newRepository ifNotNil: [ :repo | 
		MCRepositoryGroup default addRepository: repo.
		remotes roots: self buildRoots ]
]

{ #category : #accessing }
RemotesManager >> addRemoteWidget [
	^ addRemote
]

{ #category : #private }
RemotesManager >> buildRoots [
	^ MCRepositoryGroup default repositories
		select: [ :each | (each isCache not) and: [ each isRemote ] ]
		thenCollect: [ :each | | remote  |			
			remote := each koRemote.
			TreeNodePresenter new
				content: remote;
				hasContentToShow: true;
				selected: ( selectedRemotes includes: each );
				whenSelectedChanged: [ :bool | self selected: each changed: bool ] ;
				yourself ]
]

{ #category : #private }
RemotesManager >> editRemote [
	| item view |
	
	item := remotes highlightedItem content.
	view := CredentialEditor new.
	view 
		setPassword: item password;
		setUsername: item username.
	
	view openDialogWithSpec
		centered;
		modalRelativeTo: self window.
	
	view cancelled ifTrue: [ ^ self ].
	
	item 
		username: view getUsername;
		password: view getPassword
]

{ #category : #accessing }
RemotesManager >> editRemoteWidget [
	^ editRemote
]

{ #category : #api }
RemotesManager >> initialExtent [

	^  400@260
]

{ #category : #initialization }
RemotesManager >> initialize [
	selectedRemotes := Set new.
	super initialize
]

{ #category : #initialization }
RemotesManager >> initializeDialogWindow: aWindow [

	aWindow
		toolbar: OkToolbar new
]

{ #category : #initialization }
RemotesManager >> initializePresenter [

	remotes whenHighlightedItemChanged: [ :item | 
		editRemote enabled: item notNil.
		removeRemote enabled: item notNil ].

]

{ #category : #initialization }
RemotesManager >> initializeWidgets [
	remotes := self newTree.
	addRemote := self newButton.
	removeRemote := self newButton.
	editRemote := self newButton.
	remotes
		beCheckList;
		removeOnlyLastSelected: true;
		autoMultiSelection: true;
		displayBlock: [ :item | item label ];
		iconBlock: [ :item | item icon ];
		roots: self buildRoots.
	addRemote
		icon: (self iconNamed: #remotesManagerAddRemote);
		action: [ self addRemote ];
		help: 'Add a new remote'.
	removeRemote
		icon: (self iconNamed: #remotesManagerRemoveRemote);
		action: [ self removeRemote ];
		enabled: false;
		help: 'Remote the selected remote'.
	editRemote
		icon: (self iconNamed: #remotesManagerEditRemote);
		action: [ self editRemote ];
		enabled: false;
		help: 'Edit the selected remote'.
	self focusOrder
		add: remotes;
		add: removeRemote;
		add: addRemote
]

{ #category : #accessing }
RemotesManager >> remotes [
	^ remotes
]

{ #category : #private }
RemotesManager >> removeRemote [
	| item |
	
	item := remotes highlightedItem content.
	MCRepositoryGroup default
		removeRepository: item remote.
		
	remotes 
		roots: self buildRoots;
		highlightedItem: nil
]

{ #category : #accessing }
RemotesManager >> removeRemoteWidget [
	^ removeRemote
]

{ #category : #private }
RemotesManager >> selected: aRepository changed: selected [
	selected
		ifTrue: [ selectedRemotes add: aRepository ]
		ifFalse: [ selectedRemotes remove: aRepository ifAbsent: [  ] ].
	selectedChangedBlock ifNotNil: [ selectedChangedBlock cull: aRepository cull: selected ]
]

{ #category : #api }
RemotesManager >> selectedChangedBlock: aBlock [
	selectedChangedBlock := aBlock
]

{ #category : #accessing }
RemotesManager >> selectedRemotes [
	^ selectedRemotes
]

{ #category : #api }
RemotesManager >> selectedRemotes: aRemoteList [
	selectedRemotes := aRemoteList asSet
]

{ #category : #accessing }
RemotesManager >> title [
	^ 'Remotes manager'
]
