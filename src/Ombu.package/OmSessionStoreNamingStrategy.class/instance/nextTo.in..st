accessing
nextTo: aName in: aDirectory
	"Answer a name for a new Ombu log, that should not collide with an existing file in the same directory."
	
	^ self subclassResponsibility