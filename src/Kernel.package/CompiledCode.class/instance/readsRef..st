scanning
readsRef: literalAssociation 
	"Answer whether the receiver loads the argument."
	| litIndex scanner |
	(litIndex := self indexOfLiteral: literalAssociation) = 0 ifTrue:
		[^false].
	litIndex := litIndex - 1.
	^(scanner := InstructionStream on: self) scanFor: (self encoderClass bindingReadScanBlockFor: litIndex using: scanner)