accessing
contents
	"Answer with a copy of my collection from 1 to readLimit."

	| newArray |
	isBinary ifFalse: [^ super contents].	"String"
	readLimit := readLimit max: position.
	newArray := ByteArray new: readLimit.
	^ newArray replaceFrom: 1
		to: readLimit
		with: collection
		startingAt: 1.