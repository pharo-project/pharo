initialize
errorCodeNameFor: cm
	"Assumes the method holds a primitive with error code"
	| primitivePragma selectorParts |
	primitivePragma := cm pragmas detect: [ :pragma | 
		selectorParts := pragma keyword separateKeywords splitOn: '  '.
		selectorParts first = 'primitive:' ].
	^ primitivePragma argumentAt: (selectorParts indexOf: 'error:')