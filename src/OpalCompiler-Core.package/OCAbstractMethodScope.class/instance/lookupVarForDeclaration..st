lookup
lookupVarForDeclaration: name
	tempVars at: name ifPresent: [:v | ^ v].
	name = 'thisContext' ifTrue: [^ thisContextVar].
	^self outerScope lookupVarForDeclaration: name
	