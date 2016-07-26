as yet unclassified
computeBoundsIn: layoutBounds
	| rest oldBounds |

	self visible ifFalse: [ ^ layoutBounds ].
	
	oldBounds := bounds.
	rest := placement value: layoutBounds.
	
	oldBounds ~= bounds ifTrue: [ 
		self invalidRect: (oldBounds quickMerge: bounds)
		 ].
	^ rest