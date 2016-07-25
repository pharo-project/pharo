optimizing
absorbConstantConditionalJumps: alreadySeen
	"Collapse sequences that look like:
			[if] goto s1
			...
		 s1:	pushConst: true/false
			goto s2
		 s2:	if true/false goto s3 else s4
	into:
			[if] goto s3/s4

	These sequences are produced by and:/or: messages"

	| seq bool if |
	(alreadySeen includes: self) ifTrue: [^ self].
	alreadySeen add: self.

	[(seq := self successorSequences) notEmpty  "not return"
	  and: [(seq := seq first "destination") size = 2
	   and: [(seq first isPushLiteral: [:obj | (bool := obj) isKindOf: Boolean])
	    and: [seq last isGoto
	     and: [(if := seq last destination first) isIf]]]]
	] whileTrue: [ "absorb"
		self last destination: (bool == if boolean
			ifTrue: [if destination]
			ifFalse: [if otherwise]).
	].

	self successorSequences do: [:instrs | instrs ifNotNil: [:i | i absorbConstantConditionalJumps: alreadySeen]].

