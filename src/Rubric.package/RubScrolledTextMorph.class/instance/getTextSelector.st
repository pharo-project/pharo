model protocol
getTextSelector
	^ getTextSelector ifNil: [  getTextSelector := #getText ]