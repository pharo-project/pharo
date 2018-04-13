Extension { #name : #Stream }

{ #category : #'*Fuel-Core' }
Stream >> nextBytesPutAll: aCollection [
	"Append the bytes of aCollection to the sequence of bytes accessible 
	by the receiver. Answer aCollection."

	^self nextPutAll: aCollection
]
