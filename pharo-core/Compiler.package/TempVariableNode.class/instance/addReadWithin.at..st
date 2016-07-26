code generation (closures)
addReadWithin: scopeBlock "<BlockNode>" at: location "<Integer>"
	readingScopes ifNil: [readingScopes := Dictionary new].
	(readingScopes at: scopeBlock ifAbsentPut: [Set new]) add: location.
	remoteNode ifNotNil:
		[remoteNode addReadWithin: scopeBlock at: location]