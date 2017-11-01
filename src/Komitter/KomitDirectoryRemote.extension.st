Extension { #name : #KomitDirectoryRemote }

{ #category : #'*Komitter-UI' }
KomitDirectoryRemote >> icon [
	^ 'DIR'
]

{ #category : #'*Komitter-UI' }
KomitDirectoryRemote >> label [
	
	^ self remote directory basename
]
