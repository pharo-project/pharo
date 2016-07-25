parsing
lastMatchValueSequence: values
	"answer the last contiguous tokens that match pattern tokens, 
	or nil if not found.
	matching is case insensitive "
	| answer  nullToken match tok |
	nullToken := {''. nil. nil}.
	tokens size - values size + 1 to: 1 by: -1 do:[:ti |
		match := true.
		answer := Array new.
		1 to: values size do:[:vi |
			tok := tokens at: ti + vi - 1 ifAbsent: [nullToken].
			(match and: [tok first asLowercase = ( values at: vi) asLowercase])
				ifFalse:[match := false]
				ifTrue:[answer := answer, {tok} ]].
		match ifTrue:[^answer]].
	^nil
			
