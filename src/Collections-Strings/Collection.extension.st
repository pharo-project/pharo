Extension { #name : 'Collection' }

{ #category : '*Collections-Strings' }
Collection >> asCharacterSet [
	"Answer a CharacterSet whose elements are the unique elements of the receiver.
	The reciever should only contain characters."

	^ CharacterSet newFrom: self
]

{ #category : '*Collections-Strings' }
Collection >> asMultilineString [

	^ String streamContents: [ :stream |
		  self
			  do: [ :each | stream nextPutAll: each asString ]
			  separatedBy: [ stream nextPut: Character cr ] ]
]
