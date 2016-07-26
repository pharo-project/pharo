scanning
writesRef: literalAssociation 
	"Answer whether the receiver stores into the argument."
	| litIndex scanner |
	(litIndex := self indexOfLiteral: literalAssociation) = 0 ifTrue:
		[^false].
	litIndex := litIndex - 1.
	^(scanner := InstructionStream on: self) scanFor: (self encoderClass bindingWriteScanBlockFor: litIndex using: scanner)