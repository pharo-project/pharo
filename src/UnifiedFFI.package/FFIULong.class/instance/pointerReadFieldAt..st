private emitting code
pointerReadFieldAt: byteOffset
	"since offsetPointerReadFieldAt: receives a variable no matter what we use the trick of 
	 just passing the offset as a string... it will work :)"
	^ self offsetPointerReadFieldAt: byteOffset asString