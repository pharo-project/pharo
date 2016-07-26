private
privateSendBlock: literalStream with: distanceStream with: litTree with: distTree
	"Send the current block using the encodings from the given literal/length and distance tree"
	| lit dist code extra sum |
	<primitive: 'primitiveZipSendBlock' module: 'ZipPlugin'>
	sum := 0.
	[lit := literalStream next.
	dist := distanceStream next.
	lit == nil] whileFalse:[
		dist = 0 ifTrue:["lit is a literal"
			sum := sum + 1.
			self nextBits: (litTree bitLengthAt: lit)
				put: (litTree codeAt: lit).
		] ifFalse:["lit is match length"
			sum := sum + lit + MinMatch.
			code := (MatchLengthCodes at: lit + 1).
			self nextBits: (litTree bitLengthAt: code)
				put: (litTree codeAt: code).
			extra := ExtraLengthBits at: code-NumLiterals.
			extra = 0 ifFalse:[
				lit := lit - (BaseLength at: code-NumLiterals).
				self nextBits: extra put: lit.
			].
			dist := dist - 1.
			dist < 256
				ifTrue:[code := DistanceCodes at: dist + 1]
				ifFalse:[code := DistanceCodes at: 257 + (dist bitShift: -7)].
			"self assert:[code < MaxDistCodes]."
			self nextBits: (distTree bitLengthAt: code)
				put: (distTree codeAt: code).
			extra := ExtraDistanceBits at: code+1.
			extra = 0 ifFalse:[
				dist := dist - (BaseDistance at: code+1).
				self nextBits: extra put: dist.
			].
		].
	].
	^sum