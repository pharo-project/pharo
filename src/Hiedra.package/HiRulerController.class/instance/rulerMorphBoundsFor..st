accessing
rulerMorphBoundsFor: aNode
	| nodeIndex |
	"hack: sync with tree if needed"
	(nodes includes: aNode) ifFalse: [ self updateFromTree ].
	
	nodeIndex := self indexOfNode: aNode.

	^ 0 @ (nodeIndex - 1 * rulerBuilder rowSize) corner: rulerForm width @ ((nodeIndex * rulerBuilder rowSize))