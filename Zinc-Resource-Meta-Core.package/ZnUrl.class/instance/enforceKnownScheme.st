convenience
enforceKnownScheme
	(scheme isNil or: [ #(#http #https #ws #wss #file) includes: scheme ])
		ifFalse: [ (ZnUnknownScheme scheme: scheme) signal ]