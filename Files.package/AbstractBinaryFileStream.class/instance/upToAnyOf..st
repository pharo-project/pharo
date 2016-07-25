reading
upToAnyOf: delimiters

	^ ByteArray new: 1000 streamContents: [ :stream | | ch |
		[ (ch := self next) isNil or: [ delimiters includes: ch] ] 
			whileFalse: [ stream nextPut: ch ] ]