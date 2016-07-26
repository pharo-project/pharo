converting
nextCodePointFromStream: stream
	"Read and return the next integer code point from stream"

	| word leadSurrogate trailSurrogate code |
	word := self read16BitWordFromStream: stream.
	(self processByteOrderMark: word)
		ifTrue: [ word := self read16BitWordFromStream: stream ].
	^ (word < 16rD800 or: [ word > 16rDBFF ])
		ifTrue: [
			word ]
		ifFalse: [ 
			leadSurrogate := word.
			trailSurrogate := self read16BitWordFromStream: stream.
			code := (leadSurrogate - 16rD800) * 16r400 + (trailSurrogate - 16rDC00).
			16r10000 + code ]