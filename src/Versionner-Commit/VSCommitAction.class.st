"
I'm a commit action. 
I can be executed to perform certain operations (commit packages to their repositories, generate new configurations, etc)
"
Class {
	#name : #VSCommitAction,
	#superclass : #Object,
	#category : #'Versionner-Commit'
}

{ #category : #'*Versionner-Commit-UI' }
VSCommitAction >> browseOn: aBrowser [
	self subclassResponsibility
]

{ #category : #private }
VSCommitAction >> commitPackage: packageName comment: comment [
	| remoteRepository version |

	remoteRepository := self remoteRepository.
	version := self workingCopy 
		newVersionWithMessage: comment 
		in: remoteRepository.
	self retryOnCredentialRequest: [ remoteRepository storeVersion: version ]
]

{ #category : #'*Versionner-Commit-UI' }
VSCommitAction >> currentString [
	^ self subclassResponsibility
]

{ #category : #'*Versionner-Commit-UI' }
VSCommitAction >> displayString [
	^ self subclassResponsibility

]

{ #category : #executing }
VSCommitAction >> executeOn: aUnit [
	self subclassResponsibility
]

{ #category : #'*Versionner-Commit-UI' }
VSCommitAction >> icon [
	^ self subclassResponsibility
]

{ #category : #testing }
VSCommitAction >> isPackage [
	^ false
]

{ #category : #testing }
VSCommitAction >> isProject [
	^ false
]

{ #category : #accessing }
VSCommitAction >> nextVersion: aString [	
]

{ #category : #accessing }
VSCommitAction >> nextVersionOn: aUnit [
	^ nil
]

{ #category : #private }
VSCommitAction >> remoteRepository [
	| repositoryWithoutSlash |
	
	repositoryWithoutSlash  := self repository last = $/
		ifTrue: [ self repository allButLast ]
		ifFalse: [ self repository ].
	
	^ MCRepositoryGroup default repositories
		detect: [:each | 
			(each description = repositoryWithoutSlash) 
			or: [ each description = (repositoryWithoutSlash, '/') ] ]
		ifNone: [
			MCHttpRepository
				location: self repository
				user: ''
				password: '' ]
]

{ #category : #accessing }
VSCommitAction >> removeOn: aUnit [
	self subclassResponsibility
]

{ #category : #accessing }
VSCommitAction >> repository [
	^ self subclassResponsibility
]

{ #category : #private }
VSCommitAction >> retryOnCredentialRequest: aBlock [
	aBlock on: MCPermissionDenied do: [ :e | 
		| credentials remoteRepository | 
	
		remoteRepository := self remoteRepository.
		credentials := MCCredentialsRequest 
			signalUrl: remoteRepository location
			username: remoteRepository user
			password: remoteRepository password.
		credentials ifNotNil: [
			remoteRepository credentials: credentials.
			^ self retryOnCredentialRequest: aBlock ] ]
]

{ #category : #validation }
VSCommitAction >> validate [
]

{ #category : #private }
VSCommitAction >> workingCopy [ 
	^ self subclassResponsibility
]
