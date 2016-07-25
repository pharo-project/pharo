find-select
findNext: aRegex startingAt: searchIdx 
	| strm match range |
	strm := (self string copyFrom: searchIdx to: self string size) readStream.
	aRegex searchStream: strm.
	match := aRegex subexpression: 1.
	match ifNotNil: [range := (aRegex position + searchIdx - match size) to: (aRegex position + searchIdx - 1)].
	^ range