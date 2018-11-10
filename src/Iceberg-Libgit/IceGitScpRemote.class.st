"
A remote repository created out of a SCP url, such as 'git@github.com:npasserini/iceberg.git'.


"
Class {
	#name : #IceGitScpRemote,
	#superclass : #IceGitNetworkRemote,
	#category : #'Iceberg-Libgit-Core'
}

{ #category : #testing }
IceGitScpRemote class >> canHandleUrl: aUrl [
	"Very simplistic implementation that does not cover all cases. Either there
	is the ssh scheme or treat all non absolute paths as SSH"

	^ (self isSSHUrl: aUrl) or: [ self isAlternateSSHUrl: aUrl ]
]

{ #category : #defaults }
IceGitScpRemote class >> defaultPort [
	^ 22
]

{ #category : #testing }
IceGitScpRemote class >> isAlternateSSHUrl: aUrl [
	"Definition taken from https://stackoverflow.com/questions/14348874/git-repository-url-ssh-syntax-without-absolute-path"
	^ (aUrl includesSubstring: '://') not and: [ (aUrl beginsWith: '/') not ]
]

{ #category : #testing }
IceGitScpRemote class >> isSSHUrl: aUrl [
	^ aUrl beginsWith: 'ssh://'
]

{ #category : #'private - patches' }
IceGitScpRemote >> httpsUrl [
	^ 'https://{1}/{2}.git' format: { self host . self projectPath }

]

{ #category : #testing }
IceGitScpRemote >> isAlternateSSHUrl: aString [ 
	
	^ self class isAlternateSSHUrl: aString
]

{ #category : #testing }
IceGitScpRemote >> isSSHUrl: aString [ 
	
	^ self class isSSHUrl: aString
]

{ #category : #'private - patches' }
IceGitScpRemote >> parseUrl [
	| urlStream restSegments |
	(self isSSHUrl: url)
		ifTrue: [ ^ super parseUrl ].
	
	(self isAlternateSSHUrl: url) ifFalse: [ IceWrongUrl signal: 'Remote repository scheme not supported: ', url ].
	
	"We now parse alternate ssh urls like lars@myserver.example.com:repos/myrepo.git"
	urlStream := url readStream.
	user := urlStream upTo: $@.
	host := (urlStream upTo: $:) ifEmpty: [ nil ].
	restSegments := urlStream upToEnd substrings: '/'.
	
	path := restSegments size >= 2
		ifTrue: [ restSegments allButLast: 2 ]
		ifFalse: [ #() ].
	owner := restSegments size > 1
		ifTrue: [ restSegments at: (restSegments size - 1) ]
		ifFalse: [ nil ].

	restSegments isEmpty ifTrue: [ IceWrongUrl signal: 'Remote repository scheme not supported: ', url ].
	projectName := self stripPossibleExtension: (restSegments last)
]

{ #category : #testing }
IceGitScpRemote >> referencesSameRemoteLocationAs: aRemote [
	^ ((super referencesSameRemoteLocationAs: aRemote)
		or: [ self httpsUrl = aRemote httpsUrl ])
			
]
