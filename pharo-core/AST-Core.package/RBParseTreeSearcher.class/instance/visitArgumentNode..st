visiting
visitArgumentNode: aNode
	| value |
	value := self performSearches: argumentSearches on: aNode.
	^ value
		ifNil: [ 
			super visitArgumentNode: aNode.
			aNode ]
		ifNotNil: [ value ]