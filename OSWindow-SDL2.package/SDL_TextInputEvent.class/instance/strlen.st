accesing
strlen
	| len |
	len := 0.
	12 to: 12 + 32 - 1 do:
		[ :i | 
		(self getHandle nbUInt8AtOffset: i) = 0
			ifTrue: [ ^ len ].
		len := len + 1 ].
	^ len