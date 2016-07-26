accessing
topLeftColor
	"Changed from direct access to color since, if nil,
	self color is transparent."

	^width = 1 
		ifTrue: [self color twiceLighter]
		ifFalse: [self color lighter]