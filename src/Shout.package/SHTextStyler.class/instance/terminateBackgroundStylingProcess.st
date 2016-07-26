private
terminateBackgroundStylingProcess
	self monitor critical: [
		backgroundProcess ifNotNil: [
			backgroundProcess terminate.
			backgroundProcess := nil ] ]