instance creation
mappingFrom: srcBitMasks to: dstBitMasks 
	"Return a color map mapping from the array of source bit masks
	to the array of dest bit masks."
	| shifts masks shiftAndMask |
	shifts := IntegerArray new: 4.
	masks := WordArray new: 4.
	1 
		to: 4
		do: 
			[ :i | 
			shiftAndMask := self 
				mapBitsFrom: (srcBitMasks at: i)
				to: (dstBitMasks at: i).
			shifts 
				at: i
				put: (shiftAndMask at: 1).
			masks 
				at: i
				put: (shiftAndMask at: 2) ].
	^ self 
		shifts: shifts
		masks: masks