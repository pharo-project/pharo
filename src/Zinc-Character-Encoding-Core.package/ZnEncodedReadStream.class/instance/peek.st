accessing
peek
	^ peeked
		ifNil: [
			stream atEnd ifFalse: [ peeked := self nextElement ] ]