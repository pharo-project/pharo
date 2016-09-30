scanning
writesField: varIndex
	"Answer whether the receiver stores into the instance variable indexed by the argument."

	| varIndexCode scanner |
	varIndexCode := varIndex - 1.
	((scanner := InstructionStream on: self) scanFor: (self encoderClass instVarWriteScanBlockFor: varIndexCode using: scanner)) ifTrue: [ ^ true ].
	^ self innerCompiledBlocksAnySatisfy: [ :cb | cb writesField: varIndex ]