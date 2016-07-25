icons
lockedIcon

	^ Icons
		at: #lockedIcon
		ifAbsentPut: [ self lockedIconContents ]