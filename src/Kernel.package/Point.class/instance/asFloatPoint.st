converting
asFloatPoint
	"Convert me to a float point transforming both of my coordinates
	to floats using #asFloat. If x and y are already represented
	by floats return myself, otherwise create a new Point instance."
	self isFloatPoint
		ifTrue: [ ^ self ].
	^ x asFloat @ y asFloat