accessing
infos
	^ infos ifNil: [infos := ancestry withBreadthFirstAncestors]