computation
makeDerivedTables
	| huffSize huffCode code si index lookbits |
	mincode := Array new: 16.
	maxcode := Array new: 17.
	valptr := Array new: 17.
	huffSize := OrderedCollection new.
	1 
		to: 16
		do: 
			[ :l | 
			1 
				to: (bits at: l)
				do: [ :i | huffSize add: l ] ].
	huffSize add: 0.
	code := 0.
	huffCode := Array new: huffSize size.
	si := huffSize at: 1.
	index := 1.
	[ (huffSize at: index) ~= 0 ] whileTrue: 
		[ [ (huffSize at: index) = si ] whileTrue: 
			[ huffCode 
				at: index
				put: code.
			index := index + 1.
			code := code + 1 ].
		code := code << 1.
		si := si + 1 ].
	index := 1.
	1 
		to: 16
		do: 
			[ :l | 
			(bits at: l) ~= 0 
				ifTrue: 
					[ valptr 
						at: l
						put: index.
					mincode 
						at: l
						put: (huffCode at: index).
					index := index + (bits at: l).
					maxcode 
						at: l
						put: (huffCode at: index - 1) ]
				ifFalse: 
					[ maxcode 
						at: l
						put: -1 ] ].
	maxcode 
		at: 17
		put: 1048575.
	lookaheadBits := (Array new: 1 << Lookahead) atAllPut: 0.
	lookaheadSymbol := Array new: 1 << Lookahead.
	index := 1.
	1 
		to: Lookahead
		do: 
			[ :l | 
			1 
				to: (bits at: l)
				do: 
					[ :i | 
					lookbits := ((huffCode at: index) << (Lookahead - l)) + 1.
					(1 << (Lookahead - l) 
						to: 1
						by: -1) do: 
						[ :ctr | 
						lookaheadBits 
							at: lookbits
							put: l.
						lookaheadSymbol 
							at: lookbits
							put: (values at: index).
						lookbits := lookbits + 1 ].
					index := index + 1 ] ]