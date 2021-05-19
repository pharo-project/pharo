Extension { #name : #ZnBufferedWriteStream }

{ #category : #'*Fuel-Core' }
ZnBufferedWriteStream >> nextBytesPutAll: aCollection [
	^ self nextPutAll: aCollection
]
