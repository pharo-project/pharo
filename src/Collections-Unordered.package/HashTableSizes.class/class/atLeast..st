public
atLeast: lowerLimit
	"Answer the next good size >= lowerlimit.
	If lowerLimit is larger than the largest known good prime,
	just make it odd."

	^ lowerLimit <= self maxSmallSize
		ifTrue: [
			self smallSizesLookupTable at:
				(lowerLimit <= 0
					ifTrue: [ 1 ]
					ifFalse: [ lowerLimit ceiling + 1 ]) ]
		ifFalse: [ self basicAtLeast: lowerLimit ]