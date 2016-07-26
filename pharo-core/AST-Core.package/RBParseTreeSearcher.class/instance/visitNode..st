visiting
visitNode: aNode 
	| value |
	value := self performSearches: searches on: aNode.
	^ value
		ifNil: [ 
			super visitNode: aNode.
			aNode ]
		ifNotNil: [ value ]