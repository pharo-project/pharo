compiled method support
literalIndexOfBytecodeAt: pc in: aCompiledMethod
	"	16-31		0001 i i i i				Push Literal Variable #iiii
	 	32-63		001 i i i i i				Push Literal #iiiii
	 *	224		11100000	aaaaaaaa	Extend A (Ext A = Ext A prev * 256 + Ext A)
	 *	227		11100011	i i i i i i i i	Push Literal Variable #iiiiiiii (+ Extend A * 256)
	 *	228		11100100	i i i i i i i i	Push Literal #iiiiiiii (+ Extend A * 256)
	 *	241		11110001	iiiiiiii		Pop and Store Literal Variable #iiiiiiii (+ Extend A * 256)
	 *	244		11110100	iiiiiiii		Store Literal Variable #iiiiiiii (+ Extend A * 256)"
	| byte |
	byte := aCompiledMethod at: pc.
	byte <= 63 ifTrue:
		[byte >= 32 ifTrue:
			[^byte bitAnd: 16r1F].
		 ^byte >= 16 ifTrue:
			[byte bitAnd: 16rF]].
	(#(227 228 241 244) includes: byte) ifTrue:
		[^aCompiledMethod at: pc + 1].
	(byte = 224 "Ext A"
	 and: [#(227 228 241 244) includes: (aCompiledMethod at: pc + 2)]) ifTrue:
		[^(aCompiledMethod at: pc + 1) * 256 + (aCompiledMethod at: pc + 3)].
	^nil