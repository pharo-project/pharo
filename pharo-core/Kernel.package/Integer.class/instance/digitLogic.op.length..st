private
digitLogic: arg op: op length: len 
	| i result neg1 neg2 rneg z1 z2 rz b1 b2 b |
	neg1 := self negative.
	neg2 := arg negative.
	rneg := ((neg1
				ifTrue: [-1]
				ifFalse: [0])
				perform: op
				with: (neg2
						ifTrue: [-1]
						ifFalse: [0]))
				< 0.
	result := Integer new: len neg: rneg.
	rz := z1 := z2 := true.
	i := 0.
	[(i := i + 1) <= len
		or: ["mind a carry on result that might go past len digits"
			rneg and: [rz
				and: [result := result growby: 1.
					true]]]]
		whileTrue: [b1 := self digitAt: i.
			neg1
				ifTrue: [b1 := z1
								ifTrue: [b1 = 0
										ifTrue: [0]
										ifFalse: [z1 := false.
											256 - b1]]
								ifFalse: [255 - b1]].
			b2 := arg digitAt: i.
			neg2
				ifTrue: [b2 := z2
								ifTrue: [b2 = 0
										ifTrue: [0]
										ifFalse: [z2 := false.
											256 - b2]]
								ifFalse: [255 - b2]].
			b := b1 perform: op with: b2.
			result
				digitAt: i
				put: (rneg
						ifTrue: [rz
								ifTrue: [b = 0
										ifTrue: [0]
										ifFalse: [rz := false.
											256 - b]]
								ifFalse: [255 - b]]
						ifFalse: [b])].
	^ result normalize