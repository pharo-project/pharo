Extension { #name : #MCRepositoryGroup }

{ #category : #'*Komitter-Models' }
MCRepositoryGroup >> remotes [
	^ self repositories reject: [ :each | each isCache ]
]
