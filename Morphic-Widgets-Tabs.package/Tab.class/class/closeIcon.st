icons
closeIcon

	^ Icons
		at: #closeIcon
		ifAbsentPut: [ self closeIconContents ]