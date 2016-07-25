public
value: anObject 
	"Allows passing generators as arguments to methods expecting blocks.
	A synonym for #yield: / #nextPut:."
	^ self nextPut: anObject