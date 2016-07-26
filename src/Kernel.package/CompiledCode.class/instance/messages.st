scanning
messages
	"Answer a Set of all the message selectors sent by this method."

	| scanner aSet |
	aSet := IdentitySet new.
	scanner := InstructionStream on: self.
	scanner	scanFor: [:x | 
			scanner addSelectorTo: aSet.
			false	"keep scanning"].
	^aSet