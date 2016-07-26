computation
valueForCode: code length: length

	^ values at: ((valptr at: length) + code - (mincode at: length))