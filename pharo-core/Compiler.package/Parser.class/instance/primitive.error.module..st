primitives
primitive: aNameString error: errorCodeVariableOrNil module: aModuleStringOrNil
	"Create named primitive with optional error code."
	
	<primitive>
	^self primitive: aNameString module: aModuleStringOrNil error: errorCodeVariableOrNil