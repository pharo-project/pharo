initialize
initialize

	super initialize.

	encoder := self encoderClass new.
	encoder stream: self.
	
	primNumber := 0.
	
	literals := OCLiteralList new.
	"The following dicts are keyed by sequence id given by client in label: (and gotos)."
	seqOrder := IdentityDictionary new.  "seqId -> seq order num"
	seqBytes := IdentityDictionary new.  "seqId -> seq bytecodes"
	jumps := IdentityDictionary new.  "seqId -> last jump instr"
	instrMaps := IdentityDictionary new.  "seqId -> (clientInstr -> bytecode pos)"
	stacks := IdentityDictionary new.  "seqId -> stackCount"
	numArgs := 0.
	currentSeqNum := 0.
	orderSeq := OrderedCollection new.  "reverse map of seqOrder"
	additionalLiterals := OCLiteralSet new.
	forceLongForm := false.
	"starting label in case one is not provided by client"
	self label: self newDummySeqId.
	

	