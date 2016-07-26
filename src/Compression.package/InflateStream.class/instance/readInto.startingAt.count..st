accessing
readInto: buffer startingAt: startIndex count: n
	"Read n objects into the given collection. 
	Return number of elements that have been read."
	| c numRead count |
	n = 0 ifTrue: [ ^n ].
	numRead := 0.
	["Force decompression if necessary"
	(c := self next) == nil 
		ifTrue: [^numRead].
	"Store the first value which provoked decompression"
	buffer at: startIndex + numRead put: c.
	numRead := numRead + 1.
	"After collection has been filled copy as many objects as possible"
	count := (readLimit - position) min: (n - numRead).
	buffer 
		replaceFrom: startIndex + numRead 
		to: startIndex + numRead + count - 1 
		with: collection 
		startingAt: position+1.
	position := position + count.
	numRead := numRead + count.
	numRead = n] whileFalse.
	^n