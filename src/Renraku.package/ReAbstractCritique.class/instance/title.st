accessing
title
	^ (self tinyHint isNil or: [ self tinyHint isEmpty ])
		ifTrue: [ rule name ]
		ifFalse: [
			String streamContents: [ :s |
				s
					nextPut: $[;
					nextPutAll: self tinyHint;
					nextPutAll: '] ';
					nextPutAll: rule name ] ]