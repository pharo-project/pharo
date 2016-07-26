transmitting
transmitIn: aContext
	self flag: 'The transmitIt: and receive:in: both end up calling notingPresentationChangeDo:. So they should be handled by a surrounding notingPresentationChangeDo:. However, by doing that it seems that we break the morphic updating. No idea why'.
	self destination notingPresentationChangeDo: [
		self transmissionStrategy transmitIn: aContext.
		self destination receive: self in: aContext ].
	self browser notNil ifTrue: [
		self browser announce: GLMTransmissionTriggered new ]