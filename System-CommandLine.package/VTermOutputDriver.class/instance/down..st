navigating
down: distance
	distance < 0
		ifTrue: [ 0 - distance timesRepeat: [ self up ]]
		ifFalse: [ distance timesRepeat: [ self down ]].