converting
backOnStream: stream
	"Move back one character on stream"

	| word |
	word := self back16BitWordOnStream: stream.
	(word < 16rD800 or: [ word > 16rDBFF ])
		ifFalse: [ 
			self back16BitWordOnStream: stream ]