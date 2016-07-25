model protocol
setTextSelector
	^ setTextSelector ifNil: [  setTextSelector := #setText:from:]