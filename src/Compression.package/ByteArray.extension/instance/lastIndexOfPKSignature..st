*Compression
lastIndexOfPKSignature: aSignature
	"Answer the last index in me where aSignature (4 bytes long) occurs, or 0 if not found"
	| a b c d |
	a := aSignature first.
	b := aSignature second.
	c := aSignature third.
	d := aSignature fourth.
	(self size - 3) to: 1 by: -1 do: [ :i |
		(((self at: i) = a)
			and: [ ((self at: i + 1) = b)
				and: [ ((self at: i + 2) = c)
					and: [ ((self at: i + 3) = d) ]]])
						ifTrue: [ ^i ]
	].
	^0