private-exceptions
restartWithNewReceiver: obj

	self
		swapReceiver: obj;
		restart