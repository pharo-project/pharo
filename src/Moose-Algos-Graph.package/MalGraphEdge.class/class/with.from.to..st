instance creation
with: aModel from: srcNode to: dstNode
	| edge |
	edge := self new.
	edge model: aModel.
	edge from: srcNode.
	edge to: dstNode.
	^ edge.