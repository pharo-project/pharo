navigating
up: distance
	distance < 0
		ifTrue: [ 0 - distance timesRepeat: [ self down ]]
		ifFalse: [ distance timesRepeat: [ self up ]].