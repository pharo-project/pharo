accessing
associations
	"Answer a collection containing the receiver's associations."
	"Suggested by l. Uzonyi"
	
	^Array new: self size streamContents: [ :stream |
		self associationsDo: [ :each | stream nextPut: each ] ]