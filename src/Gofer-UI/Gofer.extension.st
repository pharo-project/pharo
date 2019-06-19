Extension { #name : #Gofer }

{ #category : #'*Gofer-UI' }
Gofer >> browseLocalChanges [
	"Browse the changes between the base version and the working copy."
	self deprecated: 'I am an unused UI' on: '6/9/2019' in: #Pharo8.
	^ self execute: GoferBrowseLocalChanges
]

{ #category : #'*Gofer-UI' }
Gofer >> browseRemoteChanges [
	"Browse the changes between the working copy and the remote changes."
	self deprecated: 'I am an unused UI' on: '6/9/2019' in: #Pharo8.
	^ self execute: GoferBrowseRemoteChanges
]
