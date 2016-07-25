querying
unclassified
	^ self nodeList select: [:node | node parentName notNil and: [node parentNode isNil]].