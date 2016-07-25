initialization
initialize
	ir := IRMethod new.
	jumpAheadStacks := IdentityDictionary new.
	jumpBackTargetStacks := IdentityDictionary new.
	sourceMapNodes := OrderedCollection new.	"stack"
	currentScope := Stack new.
	self pushScope: ir.
	
	"Leave an empty sequence up front (guaranteed not to be in loop)"
	ir startSequence: ((IRSequence orderNumber: 0) method:ir).
	currentSequence := (IRSequence orderNumber: 1) method:ir.
	ir startSequence add:
		(IRJump new
			destination: currentSequence;
			bytecodeIndex: sourceMapByteIndex;
			yourself).
