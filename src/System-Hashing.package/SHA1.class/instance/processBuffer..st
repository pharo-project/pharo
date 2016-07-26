private
processBuffer: aByteArray 
	"Process given 64-byte buffer, accumulating the results in totalA through totalE."
	| a b c d e w tmp |
	self primHasSecureHashPrimitive 
		ifTrue: [ ^ self processBufferUsingPrimitives: aByteArray ]
		ifFalse: [ totals := nil ].

	"initialize registers a through e from the current totals"
	a := totalA copy.
	b := totalB copy.
	c := totalC copy.
	d := totalD copy.
	e := totalE copy.

	"expand and process the buffer"
	w := self expandedBlock: aByteArray.
	1 
		to: 80
		do: 
			[ :i | 
			tmp := (a copy leftRotateBy: 5)
				+= (self 
						hashFunction: i
						of: b
						with: c
						with: d);
				+= e;
				+= (w at: i);
				+= (self constantForStep: i).
			e := d.
			d := c.
			c := b copy leftRotateBy: 30.
			b := a.
			a := tmp ].

	"add a through e into total accumulators"
	totalA += a.
	totalB += b.
	totalC += c.
	totalD += d.
	totalE += e