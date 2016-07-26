accessing
wordBefore: anIndex 
	| sep tok |
	tok := false.
	sep := anIndex.
	[ sep > 0 and: [ (self at: sep) tokenish ] ] whileTrue: 
		[ tok := true.
		sep := sep - 1 ]. 
	^ tok 
		ifTrue: 
			[ self 
				copyFrom: sep + 1
				to: anIndex ]
		ifFalse: [ String new ]