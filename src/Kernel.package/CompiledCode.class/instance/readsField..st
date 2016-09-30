scanning
readsField: varIndex 
	"Answer whether the receiver loads the instance variable indexed by the argument."
	| varIndexCode scanner |
	varIndexCode := varIndex - 1.
	((scanner := InstructionStream on: self) scanFor: (self encoderClass instVarReadScanBlockFor: varIndexCode using: scanner)) ifTrue: [ ^ true ].
	^ self innerCompiledBlocksAnySatisfy: [ :cb | cb readsField: varIndex ]