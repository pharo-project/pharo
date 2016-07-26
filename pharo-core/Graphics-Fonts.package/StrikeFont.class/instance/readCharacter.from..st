multibyte character methods
readCharacter: aBits from: aStream 
	| pos |
	pos := 0.
	12 timesRepeat: 
		[ 1 
			to: 2
			do: 
				[ :w | 
				aBits 
					byteAt: pos + w
					put: aStream next ].
		pos := pos + 4 ]