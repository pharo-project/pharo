Extension { #name : #MCHttpRepository }

{ #category : #'*GT-SpotterExtensions-Core' }
MCHttpRepository >> gtDisplayOn: aStream [
	aStream nextPutAll: self location
]
