lookup
lookupVar: name
	copiedVars at: name ifPresent: [:v | ^ v].
	tempVector  at: name ifPresent: [:v | ^ v].
	tempVars at: name ifPresent: [:v | ^ v].
	name = 'thisContext' ifTrue: [^ thisContextVar].
	^self outerScope lookupVar: name
	