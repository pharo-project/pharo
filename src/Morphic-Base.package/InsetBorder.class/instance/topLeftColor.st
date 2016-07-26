accessing
topLeftColor
	"Changed from direct access to color since, if nil,
	self color is transparent."
	
	^width = 1 
		ifTrue: [self color twiceDarker]
		ifFalse: [self color darker]