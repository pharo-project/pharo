Extension { #name : #MCDirectoryRepository }

{ #category : #'*GT-SpotterExtensions-Core' }
MCDirectoryRepository >> gtDisplayOn: aStream [
	aStream nextPutAll: self description
]
