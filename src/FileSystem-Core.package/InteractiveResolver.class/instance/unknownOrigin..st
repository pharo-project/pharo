resolving
unknownOrigin: origin
	| reference |
	^ (next ifNotNil: [next resolve: origin]) ifNil: 
			[reference := ResolutionRequest for: origin.
			reference ifNotNil: [cache at: origin put: reference]]