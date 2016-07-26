decoding
interpretV3PlusClosureJumpIfCond

	| byte |
	byte := self method at: pc.
	(byte between: 152 and: 159) ifTrue:
		[pc := pc + 1. ^byte - 151].
	(byte between: 168 and: 175) ifTrue:
		[pc := pc + 2. ^(byte bitAnd: 3) * 256 + (self method at: pc - 1)].
	^nil