testing - keys
includesKey: key
	"Answer whether the receiver has a key equal to the argument, key."

	^ (self findIndexForKey: key) ~= 0