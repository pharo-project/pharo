Extension { #name : #MCRepository }

{ #category : #'*GT-SpotterExtensions-Core' }
MCRepository >> gtDisplayOn: aStream [
	aStream nextPutAll: self description
]
