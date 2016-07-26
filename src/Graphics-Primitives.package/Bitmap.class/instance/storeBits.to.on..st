filing
storeBits: startBit to: stopBit on: aStream 
	"Store my bits as a hex string, breaking the lines every 100 bytes or 
	so to comply with the maximum line length limits of Postscript (255 
	bytes). "
	| lineWidth |
	lineWidth := 0.
	self
		do: [:word | 
			startBit
				to: stopBit
				by: -4
				do: [:shift | 
					aStream nextPut: (word >> shift bitAnd: 15) asHexDigit.
					lineWidth := lineWidth + 1].
			(lineWidth > 100)
				ifTrue: [aStream cr.
					lineWidth := 0]].
	lineWidth > 0 ifTrue: [ aStream cr ].