printing
printOn: aStream
	aStream print: offset; space.
	aStream nextPut: $<.
	bytes do: [:each | | code |
		code := each radix: 16.
			aStream nextPut: (code size < 2
				ifTrue: [$0]
				ifFalse: [code at: 1]).
			aStream nextPut: code last] separatedBy: [ aStream space].
	aStream nextPut: $>;space.
	aStream nextPutAll: description