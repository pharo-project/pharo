private
findFirstNonASCIIIn: string startingAt: offset
	"This calls a fast primitive. Note that string can be a ByteString or ByteArray"
	
	offset > string size ifTrue: [ ^ 0 ].
	^ ByteString 
		findFirstInString: string 
		inSet: ByteASCIISet 
		startingAt: offset