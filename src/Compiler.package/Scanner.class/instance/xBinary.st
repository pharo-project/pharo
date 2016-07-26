multi-character scans
xBinary

	| startOfToken |
	tokenType := #binary.
	startOfToken := mark.
	token := String with: self step.
	[(self typeTableAt: hereChar) == #xBinary] whileTrue:
		[(hereChar == $- and: [(self typeTableAt: aheadChar) == #xDigit])
			ifTrue: [^self ambiguousSelector: (token , '-')
					inRange: (startOfToken to: source position - 1).].
		token := token, (String with: self step)].
	token := token asSymbol