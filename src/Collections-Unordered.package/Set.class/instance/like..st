accessing
like: anObject
	"Answer an object in the receiver that is equal to anObject,
	nil if no such object is found. Relies heavily on hash properties.
	Note, use #like:ifAbsent: if you need to match against nil as element"

	^ self like: anObject ifAbsent: [ nil ]