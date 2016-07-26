building - graph
addNodeFor: aModel
	^ self 
		findNode: aModel
		ifAbsent: [ nodes add: (self nodeClass with: aModel) ]