temps
bindBlockArg: name within: aBlockNode
	"Read the comment in the superclass's method.
	 If we have closures we should check the argument
	 count against the block, not the method.

	(Note that this isn't entirely adequate either since optimized blocks
	 will slip through the cracks (their arguments (i.e. ifNotNil: [:expr|)
	 are charged against their enclosing block, not themselves))."
	| nArgs |
	self supportsClosureOpcodes ifFalse:
		[^super bindBlockArg: name within: aBlockNode].
	(nArgs := aBlockNode nArgsSlot) ifNil:
		[aBlockNode nArgsSlot: (nArgs := 0)].
	nArgs  >= 15 ifTrue:
		[^self notify: 'Too many arguments'].
	aBlockNode nArgsSlot: nArgs + 1.
	^(self bindTemp: name)
		beBlockArg;
		nowHasDef;
		nowHasRef;
		yourself