access
string 
	| string |
	file openReadOnly.
	file position: position.
	string := file nextChunk.
	file close.
	^ string