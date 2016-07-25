printing
printSeparatedBy: aDelimiter every: offset signed: printSigned base: base on: aStream
	| digits |
	digits := self abs printStringBase: base.
	
	self sign = -1 
		ifTrue: [ aStream nextPut: $- ] 
		ifFalse: [ printSigned ifTrue: [ aStream nextPut: $+ ] ].
	
	1 to: digits size do: [ :i | 
		aStream nextPut: (digits at: i).
		(i < digits size and: [ (i - digits size) \\ offset = 0 ])
			ifTrue: [ aStream nextPut: aDelimiter ] ]