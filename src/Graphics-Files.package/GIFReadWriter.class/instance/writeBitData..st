private-encoding
writeBitData: bits 
	"using modified Lempel-Ziv Welch algorithm."
	| maxBits maxMaxCode tSize initCodeSize ent tShift fCode pixel index disp nomatch |
	pass := 0.
	xpos := 0.
	ypos := 0.
	rowByteSize := (width * 8 + 31) // 32 * 4.
	remainBitCount := 0.
	bufByte := 0.
	bufStream := (ByteArray new: 256) writeStream.
	maxBits := 12.
	maxMaxCode := 1 bitShift: maxBits.
	tSize := 5003.
	prefixTable := Array new: tSize.
	suffixTable := Array new: tSize.
	initCodeSize := bitsPerPixel <= 1 
		ifTrue: [ 2 ]
		ifFalse: [ bitsPerPixel ].
	self nextPut: initCodeSize.
	self setParameters: initCodeSize.
	tShift := 0.
	fCode := tSize.
	[ fCode < 65536 ] whileTrue: 
		[ tShift := tShift + 1.
		fCode := fCode * 2 ].
	tShift := 8 - tShift.
	1 
		to: tSize
		do: 
			[ :i | 
			suffixTable 
				at: i
				put: -1 ].
	self writeCodeAndCheckCodeSize: clearCode.
	ent := self readPixelFrom: bits.
	[ (pixel := self readPixelFrom: bits) == nil ] whileFalse: 
		[ fCode := (pixel bitShift: maxBits) + ent.
		index := ((pixel bitShift: tShift) bitXor: ent) + 1.
		(suffixTable at: index) = fCode 
			ifTrue: [ ent := prefixTable at: index ]
			ifFalse: 
				[ nomatch := true.
				(suffixTable at: index) >= 0 ifTrue: 
					[ disp := tSize - index + 1.
					index = 1 ifTrue: [ disp := 1 ].
					"probe"
					
					[ (index := index - disp) < 1 ifTrue: [ index := index + tSize ].
					(suffixTable at: index) = fCode ifTrue: 
						[ ent := prefixTable at: index.
						nomatch := false
						"continue whileFalse:" ].
					nomatch and: [ (suffixTable at: index) > 0 ] ] whileTrue: 
						[ "probe"
						 ] ].
				"nomatch"
				nomatch ifTrue: 
					[ self writeCodeAndCheckCodeSize: ent.
					ent := pixel.
					freeCode < maxMaxCode 
						ifTrue: 
							[ prefixTable 
								at: index
								put: freeCode.
							suffixTable 
								at: index
								put: fCode.
							freeCode := freeCode + 1 ]
						ifFalse: 
							[ self writeCodeAndCheckCodeSize: clearCode.
							1 
								to: tSize
								do: 
									[ :i | 
									suffixTable 
										at: i
										put: -1 ].
							self setParameters: initCodeSize ] ] ] ].
	prefixTable := suffixTable := nil.
	self writeCodeAndCheckCodeSize: ent.
	self writeCodeAndCheckCodeSize: eoiCode.
	self flushCode.
	self nextPut: 0	"zero-length packet"