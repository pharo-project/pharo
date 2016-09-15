private
processLink: aLink
	| checkpoints |
	links add: aLink.
	
	checkpoints := OrderedCollection new.

	"add origin checkpoint"
	checkpoints add: aLink origin rulerPoint.
	
	"add checkpoints in the middle"
	aLink intermediateIndices do: [ :rowIndex |
		checkpoints add: (freeColumnIndexByRow at: rowIndex) @ rowIndex.

		self indentAt: rowIndex ].
		
	"add target checkpoint"
	checkpoints add: aLink target rulerPoint.

	"register checkpoints"
	 aLink checkpoints: checkpoints