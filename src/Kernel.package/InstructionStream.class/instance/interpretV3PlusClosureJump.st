decoding
interpretV3PlusClosureJump

	| byte |
	byte := self method at: pc.
	(byte between: 144 and: 151) ifTrue:
		[pc := pc + 1. ^byte - 143].
	(byte between: 160 and: 167) ifTrue:
		[pc := pc + 2. ^(byte - 164) * 256 + (self method at: pc - 1)].
	^nil