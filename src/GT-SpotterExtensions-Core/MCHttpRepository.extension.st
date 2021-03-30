Extension { #name : #MCHttpRepository }

{ #category : #'*GT-SpotterExtensions-Core' }
MCHttpRepository >> displayStringOn: aStream [
	aStream nextPutAll: self location
]

{ #category : #'*GT-SpotterExtensions-Core' }
MCHttpRepository >> gtDisplayOn: aStream [
	aStream nextPutAll: self location
]
