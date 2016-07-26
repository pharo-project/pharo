private
addEdge: eModel from: source to: target 

	| edge sourceNode targetNode unknownNode |
	unknownNode := false.
	sourceNode := self 	findNode: (source value: eModel)
							ifAbsent: [ unknownNode := true ].
	targetNode := self 	findNode: (target value: eModel)
							ifAbsent: [ unknownNode := true ].

	unknownNode ifFalse: [
		self edgeClass ifNil: [
			sourceNode to: targetNode.
			targetNode from: sourceNode.
			^ nil
		] ifNotNil: [
			edge := self edgeClass with: eModel.
			sourceNode to: targetNode edge: edge.
			targetNode from: sourceNode edge: edge.
			edge from: sourceNode.
			edge to: targetNode.
			self edges add: edge.	
			^ edge
		]		
	].
	^ nil