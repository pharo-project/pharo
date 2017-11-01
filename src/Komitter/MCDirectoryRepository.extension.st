Extension { #name : #MCDirectoryRepository }

{ #category : #'*Komitter-Models' }
MCDirectoryRepository >> koRemote [
	
	^ KomitDirectoryRemote new
		remote: self;
		yourself
]
