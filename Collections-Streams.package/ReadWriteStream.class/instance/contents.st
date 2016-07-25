accessing
contents
	"Answer with a copy of my collection from 1 to readLimit."

	readLimit := readLimit max: position.
	^collection copyFrom: 1 to: readLimit