scanning
readsField: varIndex 
	"Answer whether the receiver loads the instance variable indexed by the argument."
	| varIndexCode scanner |
	varIndexCode := varIndex - 1.
	self isReturnField ifTrue: [^self returnField = varIndexCode].
	^(scanner := InstructionStream on: self) scanFor: (self encoderClass instVarReadScanBlockFor: varIndexCode using: scanner)