encoding
buildHierarchyFrom: aHeap
	"Build the node hierarchy based on the leafs in aHeap"
	| left right parent |
	[aHeap size > 1] whileTrue:[
		left := aHeap removeFirst.
		right := aHeap removeFirst.
		parent := ZipEncoderNode value: -1 
			frequency: (left frequency + right frequency)
			height: (left height max: right height) + 1.
		left parent: parent.
		right parent: parent.
		parent left: left.
		parent right: right.
		aHeap add: parent].
	^aHeap removeFirst
