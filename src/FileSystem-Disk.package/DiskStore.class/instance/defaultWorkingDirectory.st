accessing
defaultWorkingDirectory
	| pathString |
	pathString := Primitives decode: Primitives imageFile.
	^ (self pathFromString: pathString) parent