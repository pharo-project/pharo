accessing
notFoundIcon
	^ self icons
		at: #notFoundIcon
		ifAbsentPut: [ Color red iconOrThumbnailOfSize: 16 ]