private socket
socketWaitForSendDone
	^ socket waitForSendDoneFor: self timeout
	