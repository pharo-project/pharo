mimic collection
first: anInteger
	^ self items copyFrom: 1 to: (self size min: (anInteger max: 0))