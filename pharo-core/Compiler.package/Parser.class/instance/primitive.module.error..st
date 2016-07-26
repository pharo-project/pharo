primitives
primitive: aNameString module: aModuleStringOrNil error: errorCodeVariableOrNil
	"Create named primitive with optional error code."
	
	<primitive>
	(aNameString isString and: [ aModuleStringOrNil isNil or: [ aModuleStringOrNil isString ] ])
		ifFalse: [ ^ self expected: 'Named primitive' ].
	self allocateLiteral: (Array 
		with: (aModuleStringOrNil isNil 
			ifFalse: [ aModuleStringOrNil asSymbol ])
		with: aNameString asSymbol
		with: 0 with: 0).
	errorCodeVariableOrNil ifNotNil:
		[encoder floatTemp: (encoder bindTemp: errorCodeVariableOrNil) nowHasDef].
	^117