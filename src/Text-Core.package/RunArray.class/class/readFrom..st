instance creation
readFrom: aStream
	"Answer an instance of me as described on the stream, aStream."

	| size runs values |
	size := aStream nextWord.
	runs := Array new: size.
	values := Array new: size.
	1 to: size do:
		[:x |
		runs at: x put: aStream nextWord.
		values at: x put: aStream nextWord].
	^ self runs: runs values: values