navigating
right: distance
	distance < 0
		ifTrue: [ 0 - distance timesRepeat: [ self left ]]
		ifFalse: [ distance timesRepeat: [ self right ]].