accessing
readInto: byteArray startingAt: startIndex count: count
	"Read up to n bytes into the given byteArray.
	Return the number of bytes actually read."
	
	| newN newStartIndex available|
	available := 0.
	collection 
		ifNil: [ 
			newN := count.
			newStartIndex := startIndex ]
		ifNotNil: [
			byteArray class isBytes 
				ifFalse: [ 
					position < readLimit ifTrue: [ self flushReadBuffer ].
					newN := count.
					newStartIndex := startIndex ]
				ifTrue: [
					(available := readLimit - position) > 0 
						ifTrue: [
							| bufferedCount |
							bufferedCount := count min: available.
							byteArray
								replaceFrom: startIndex
								to: startIndex + bufferedCount - 1
								with: collection
								startingAt: position + 1.
							position := position + bufferedCount.
							bufferedCount = count ifTrue: [ ^count ] ].
					newN := count - available.
					newStartIndex := startIndex + available ] ].
	^available + (self primRead: fileID into: byteArray
				startingAt: newStartIndex count: newN).
