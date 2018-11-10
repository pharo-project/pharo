"
I'm a presenter to define remote repositories.
Repositories needs to have a valid url (one that can be parsed by IceRemote) and a valid location (where to place the clone)
"
Class {
	#name : #IceTipGitRepositoryPanel,
	#superclass : #IceTipRepositoryTypePanel,
	#category : #'Iceberg-TipUI-View-RepositoryType'
}

{ #category : #specs }
IceTipGitRepositoryPanel class >> defaultSpec [
	<spec: #default>

	^ SpecLayout composed 
		newColumn: [ :col | 
			col
				newRow: [ :row | 
						row 
							add: #remoteLabel width: self iceLabelWidth;
							add: #remoteInputText ]
					height: self inputTextHeight;
				newRow: #projectLocation
					height: self inputTextHeight;
				newRow: #spacePanel ]
]

{ #category : #accessing }
IceTipGitRepositoryPanel class >> icon [
	^ self iconNamed: #git
]

{ #category : #accessing }
IceTipGitRepositoryPanel class >> order [
	^ 1000 "to the end"
]

{ #category : #specs }
IceTipGitRepositoryPanel class >> title [
	^ 'Clone remote repository'
]

{ #category : #utilities }
IceTipGitRepositoryPanel >> extractProjectName: aString [

	^ (IceGitRemote url: aString) projectName
]

{ #category : #initialization }
IceTipGitRepositoryPanel >> initializeWidgets [
	super initializeWidgets.
	self initializeRemoteURL.
	self remoteInputText ghostText: 'e.g., ssh://[user@]host.xz[:port]/path/to/repo.git'.
	
	self remoteInputText 	whenTextChanged: [ :text |
		self projectLocation appendPath: (self extractProjectName: text) ].
	
	self focusOrder 
		add: self remoteInputText	
]

{ #category : #accessing }
IceTipGitRepositoryPanel >> newRepository [
	UIManager default 
		informUser: ('Cloning repository {1}' format: { self remoteUrl })
		during: [ 
			^ IceRepositoryCreator new 
				repository: repository;
				remote: (IceGitRemote url: self remoteUrl);
				location: self projectLocation location;
				createRepository ]
]

{ #category : #accessing }
IceTipGitRepositoryPanel >> remoteUrl [
	^ self remoteInputText text asString trimmed
]

{ #category : #accessing }
IceTipGitRepositoryPanel >> validate [
	| remoteString |
	
	remoteString := self remoteUrl.
	self 
		assert: remoteString isNotEmpty  
		description: 'You must enter your project url.'.
	self 
		assert: (IceGitRemote isValidUrl: remoteString)
		description: 'The url is incorrect.'.
		
	self
		assert: self projectLocation location notNil  
		description: 'Project location must be defined (if it does not exists, it will be created).'
]
