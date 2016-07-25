private
init: sourceStream notifying: req failBlock: aBlock

	requestor := req.
	failBlock := aBlock.
	requestorOffset := 0.
	super scan: sourceStream.
	prevMark := hereMark := mark.
	self advance