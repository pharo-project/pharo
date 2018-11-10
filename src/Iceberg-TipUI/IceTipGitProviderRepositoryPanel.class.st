Class {
	#name : #IceTipGitProviderRepositoryPanel,
	#superclass : #IceTipGitRepositoryPanel,
	#instVars : [
		'userNameLabel',
		'userNameInputText',
		'projectNameLabel',
		'projectNameInputText',
		'protocolDropList',
		'protocolLabel'
	],
	#category : #'Iceberg-TipUI-View-RepositoryType'
}

{ #category : #specs }
IceTipGitProviderRepositoryPanel class >> defaultSpec [
	<spec: #default>

	^ SpecLayout composed 
		newColumn: [ :col | 
			col
				newRow: [ :row | 
						row 
							add: #userNameLabel width: self iceLabelWidth;
							add: #userNameInputText ]
					height: self inputTextHeight;
				newRow: [ :row | 
						row 
							add: #projectNameLabel width: self iceLabelWidth;
							add: #projectNameInputText ]
					height: self inputTextHeight;
				newRow: #projectLocation
					height: self inputTextHeight;
				newRow: [ :row | 
						row 
							add: #protocolLabel width: self iceLabelWidth;
							add: #protocolDropList ]
					height: self inputTextHeight;
				newRow: #spacePanel ]
]

{ #category : #testing }
IceTipGitProviderRepositoryPanel class >> isAbstract [
	^ super isAbstract or: [ self = IceTipGitProviderRepositoryPanel ]
]

{ #category : #accessing }
IceTipGitProviderRepositoryPanel class >> providerName [
	self subclassResponsibility
]

{ #category : #actions }
IceTipGitProviderRepositoryPanel >> configureBuilder: aBuilder [

	self subclassResponsibility
]

{ #category : #initialization }
IceTipGitProviderRepositoryPanel >> initializeWidgets [
	self initializeProjectLocation.
	self initializeSpacePanel.
	userNameLabel := self newLabel label: 'Owner name'.
	userNameInputText := self newTextInput
		autoAccept: true;
		ghostText: 'e.g., JohnDoe'.
	projectNameLabel := self newLabel label: 'Project name'.
	projectNameInputText := self newTextInput
		autoAccept: true;
		ghostText: 'e.g., MyProject'.

	protocolLabel := self newLabel label: 'Protocol'.
	protocolDropList := self newDropList.
	
	userNameInputText 	whenTextChanged: [ :text | 
		projectLocation appendPath: self projectAsPathToAppend ].
	projectNameInputText whenTextChanged: [ :text |
		projectLocation appendPath: self projectAsPathToAppend ].

	protocolDropList
		items: IceUrlProtocol allSubclasses;
		displayBlock: [ :each | each description ];
		setSelectedItem: IceUrlProtocol defaultProtocol.
	
	self focusOrder 
		add: self userNameInputText;
		add: self projectNameInputText;
		add: self projectLocation;
		add: self protocolDropList
]

{ #category : #private }
IceTipGitProviderRepositoryPanel >> projectAsPathToAppend [
	(self userName isEmpty and: [ self projectName isEmpty ])
		ifTrue: [ ^ '' ]. 
	^ (self userName ifEmpty: [ '?' ]), '/', self projectName
]

{ #category : #accessing }
IceTipGitProviderRepositoryPanel >> projectName [
	^ self projectNameInputText text asString trimmed
]

{ #category : #'accessing ui' }
IceTipGitProviderRepositoryPanel >> projectNameInputText [
	^ projectNameInputText
]

{ #category : #'accessing ui' }
IceTipGitProviderRepositoryPanel >> projectNameInputText: anObject [
	projectNameInputText := anObject
]

{ #category : #'accessing ui' }
IceTipGitProviderRepositoryPanel >> projectNameLabel [
	^ projectNameLabel
]

{ #category : #'accessing ui' }
IceTipGitProviderRepositoryPanel >> projectNameLabel: anObject [
	projectNameLabel := anObject
]

{ #category : #accessing }
IceTipGitProviderRepositoryPanel >> protocolDropList [
	^ protocolDropList
]

{ #category : #accessing }
IceTipGitProviderRepositoryPanel >> protocolDropList: anObject [
	protocolDropList := anObject
]

{ #category : #accessing }
IceTipGitProviderRepositoryPanel >> protocolLabel [
	^ protocolLabel
]

{ #category : #accessing }
IceTipGitProviderRepositoryPanel >> protocolLabel: anObject [
	protocolLabel := anObject
]

{ #category : #accessing }
IceTipGitProviderRepositoryPanel >> providerName [
	^ self class providerName
]

{ #category : #accessing }
IceTipGitProviderRepositoryPanel >> remoteUrl [
	
	| builder |
	builder := IceUrlBuilder new
		protocol: self selectedProtocol new;
		username: self userName project: self projectName.
	self configureBuilder: builder.
	^ builder buildUrl
]

{ #category : #accessing }
IceTipGitProviderRepositoryPanel >> selectedProtocol [
	^ self protocolDropList selectedItem
]

{ #category : #accessing }
IceTipGitProviderRepositoryPanel >> userName [
	^ self userNameInputText text asString trimmed
]

{ #category : #'accessing ui' }
IceTipGitProviderRepositoryPanel >> userNameInputText [
	^ userNameInputText
]

{ #category : #'accessing ui' }
IceTipGitProviderRepositoryPanel >> userNameInputText: anObject [
	userNameInputText := anObject
]

{ #category : #'accessing ui' }
IceTipGitProviderRepositoryPanel >> userNameLabel [
	^ userNameLabel
]

{ #category : #'accessing ui' }
IceTipGitProviderRepositoryPanel >> userNameLabel: anObject [
	userNameLabel := anObject
]

{ #category : #accessing }
IceTipGitProviderRepositoryPanel >> validate [
	self 
		assert: self userName isNotEmpty  
		description: 'You must enter a GitHub username.'.
	self 
		assert: self projectName isNotEmpty  
		description: 'You must enter a GitHub project.'.
	super validate
]
