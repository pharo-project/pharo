testing
hasStartUpOrShutDownMethod: aClass
	| keySelectors |
	keySelectors := #(#startUp #startUp: #shutDown #shutDown:).
	^ (aClass selectors includesAny: keySelectors)
		or: [ aClass class selectors includesAny: keySelectors ]