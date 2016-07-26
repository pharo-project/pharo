resolving
resolve: aSymbol
	^ (self canResolve: aSymbol)
		ifTrue: [self perform: aSymbol]
		ifFalse: [self unknownOrigin: aSymbol]