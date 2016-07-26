*zinc-resource-meta-core
addedToZnUrl: url
	| segments |
	segments := self findTokens: '/'.
	^ url withPathSegments: segments