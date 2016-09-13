accessing
associations
	"Answer a collection containing associations for the receiver.
	Suggested by l. Uzonyi

	Modifying the associations won't affect the receiver because it
	isn't implemented with associations like Dictionary."
	
	^Array new: self size streamContents: [ :stream |
 		self associationsDo: [ :each | stream nextPut: each ] ]