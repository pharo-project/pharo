Extension { #name : #AbstractBinaryFileStream }

{ #category : #'*Fuel' }
AbstractBinaryFileStream >> fuelNextWordsInto: aWordObject [
	"This method is the same as nextWordsInto: but the restoreEndianness is only done if needed"
	self next: aWordObject basicSize into: aWordObject.
	^ aWordObject
]
