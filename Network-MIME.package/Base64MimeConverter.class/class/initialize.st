class initialization
initialize

	FromCharTable := Array new: 256.	"nils"
	ToCharTable := Array new: 64.
	($A asciiValue to: $Z asciiValue) doWithIndex: [:val :ind | 
		FromCharTable at: val+1 put: ind-1.
		ToCharTable at: ind put: val asCharacter].
	($a asciiValue to: $z asciiValue) doWithIndex: [:val :ind | 
		FromCharTable at: val+1 put: ind+25.
		ToCharTable at: ind+26 put: val asCharacter].
	($0 asciiValue to: $9 asciiValue) doWithIndex: [:val :ind | 
		FromCharTable at: val+1 put: ind+25+26.
		ToCharTable at: ind+26+26 put: val asCharacter].
	FromCharTable at: $+ asciiValue + 1 put: 62.
	ToCharTable at: 63 put: $+.
	FromCharTable at: $/ asciiValue + 1 put: 63.
	ToCharTable at: 64 put: $/.
	