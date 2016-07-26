navigating
left: distance
	distance < 0
		ifTrue: [ 0 - distance timesRepeat: [ self right ]]
		ifFalse: [ distance timesRepeat: [ self left ]].