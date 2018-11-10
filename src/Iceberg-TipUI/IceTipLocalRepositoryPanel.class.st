"
I will define a repository that already exists. 

"
Class {
	#name : #IceTipLocalRepositoryPanel,
	#superclass : #IceTipRepositoryTypePanel,
	#category : #'Iceberg-TipUI-View-RepositoryType'
}

{ #category : #specs }
IceTipLocalRepositoryPanel class >> defaultSpec [
	<spec: #default>

	^ SpecLayout composed 
		newColumn: [ :col | 
			col
				newRow: #projectLocation
					height: self inputTextHeight;
				newRow: #spacePanel ]
]

{ #category : #accessing }
IceTipLocalRepositoryPanel class >> icon [
	^ self iconNamed: #smallOpen
]

{ #category : #accessing }
IceTipLocalRepositoryPanel class >> order [
	^ 20
]

{ #category : #specs }
IceTipLocalRepositoryPanel class >> title [
	^ 'Import from existing clone'
]

{ #category : #api }
IceTipLocalRepositoryPanel >> initialExtent [
	^ (500@(
		self class inputTextHeight * 2
		+ self class buttonHeight 
		+ 50 "Yes, a magic number"
		)) scaledByDisplayScaleFactor
]

{ #category : #initialization }
IceTipLocalRepositoryPanel >> initializeWidgets [
	super initializeWidgets.
	self focusOrder 
		add: self projectLocation
]

{ #category : #testing }
IceTipLocalRepositoryPanel >> isLocalRepository [
	^ true
]

{ #category : #accessing }
IceTipLocalRepositoryPanel >> location [
	^ self projectLocation location
]

{ #category : #accessing }
IceTipLocalRepositoryPanel >> location: aReferenceOrString [
	^ self projectLocation location: aReferenceOrString
]

{ #category : #accessing }
IceTipLocalRepositoryPanel >> newRepository [
	UIManager default 
		informUser: ('Importing repository from {1}' format: { self location fullName })
		during: [ 
			^ IceRepositoryCreator new
				repository: repository;
				location: self location;
				createRepository ]
]

{ #category : #accessing }
IceTipLocalRepositoryPanel >> validate [ 
	self
		assert: self location notNil  
		description: 'Project location must exist!'.
	
	self
		assert: self location exists  
		description: 'Project location must exist!'.
		
	self 
		assert: (IceRepositoryCreator isGitRoot: self location)
	 	description: 'Project location does not seems to be a valid git repository.'
]
