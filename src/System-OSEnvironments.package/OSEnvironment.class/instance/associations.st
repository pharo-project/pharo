accessing
associations
	"Answer a Collection containing the receiver's associations."
	
	^Array streamContents: [ :stream |
		self associationsDo: [ :each | stream nextPut: each ]]