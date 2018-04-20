Extension { #name : #StandardFileStream }

{ #category : #'*Fuel-Core' }
StandardFileStream >> fuelNextWordsInto: aWordObject [
	"This method is the same as nextWordsInto: but the restoreEndianness is only done if needed"
	self next: aWordObject basicSize into: aWordObject startingAt: 1.
	^ aWordObject
]

{ #category : #'*Fuel-Core' }
StandardFileStream >> nextBytesPutAll: aCollection [
	self nextPutAll: aCollection
]
