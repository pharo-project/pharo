Extension { #name : #Gofer }

{ #category : #'*Gofer-UI' }
Gofer >> browseLocalChanges [
	"Browse the changes between the base version and the working copy."
	
	^ self execute: GoferBrowseLocalChanges
]

{ #category : #'*Gofer-UI' }
Gofer >> browseRemoteChanges [
	"Browse the changes between the working copy and the remote changes."
	
	^ self execute: GoferBrowseRemoteChanges
]
