testing
hasWideCharacterFrom: start to: stop
	"Return true if one of my character in the range does not fit in a single byte"
	
	"Implementation note: inline #anySatisfy: here for efficiency reasons"
	^(self indexOfWideCharacterFrom: start to: stop) ~= 0