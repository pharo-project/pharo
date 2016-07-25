operation
tryMergeAndUpdatePosition: position

	"try to merge receiver with its neighbors, answer the updated position"
	
	"position is assumed to be inside of receiver"
	self assert: position span == self.
	
	self isEmpty ifTrue: [  
		self delete.
		^ prev endPosition ].
		
	^ position