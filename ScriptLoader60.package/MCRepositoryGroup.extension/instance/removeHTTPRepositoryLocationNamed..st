*ScriptLoader60
removeHTTPRepositoryLocationNamed: aRepositoryString
	
	| httpRepo others |
	httpRepo := repositories select: [:each | each isKindOf: MCHttpRepository]. 
	others := repositories reject: [:each | each isKindOf: MCHttpRepository].
	repositories := others, (httpRepo reject: [:each | each locationWithTrailingSlash = aRepositoryString])