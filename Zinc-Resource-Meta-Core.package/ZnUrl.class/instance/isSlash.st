testing
isSlash
	^ self hasPath not or: [ segments size = 1 and: [ segments first = #/ ] ]