Extension { #name : #MCPackage }

{ #category : #'*GT-SpotterExtensions-Core' }
MCPackage >> displayStringOn: aStream [
	aStream nextPutAll: self name
]

{ #category : #'*GT-SpotterExtensions-Core' }
MCPackage >> gtDisplayOn: aStream [
	aStream nextPutAll: self name
]
