encoding
buildTree: nodeList maxDepth: depth
	"Build either the literal or the distance tree"
	| heap rootNode blCounts |
	heap := Heap new: nodeList size // 3.
	heap sortBlock: self nodeSortBlock.
	"Find all nodes with non-zero frequency and add to heap"
	maxCode := 0.
	nodeList do:[:dNode|
		dNode frequency = 0 ifFalse:[
			maxCode := dNode value.
			heap add: dNode]].
	"The pkzip format requires that at least one distance code exists,
	and that at least one bit should be sent even if there is only one
	possible code. So to avoid special checks later on we force at least
	two codes of non zero frequency."
	heap size = 0 ifTrue:[
		[maxCode = 0] assert.
		heap add: nodeList first.
		heap add: nodeList second.
		maxCode := 1].
	heap size = 1 ifTrue:[
		nodeList first frequency = 0
			ifTrue:[heap add: nodeList first]
			ifFalse:[heap add: nodeList second].
		maxCode := maxCode max: 1].
	rootNode := self buildHierarchyFrom: heap.
	rootNode height > depth ifTrue:[
		rootNode := rootNode rotateToHeight: depth.
		rootNode height > depth ifTrue:[self error:'Cannot encode tree']].
	blCounts := WordArray new: depth+1.
	rootNode encodeBitLength: blCounts from: self.
	self buildCodes: nodeList counts: blCounts maxDepth: depth.
	self setValuesFrom: nodeList.