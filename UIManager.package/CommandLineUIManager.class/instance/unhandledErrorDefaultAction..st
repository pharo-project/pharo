default actions
unhandledErrorDefaultAction: anException

	self quitFrom: anException signalerContext  withMessage: anException description.
	
	UIManager default == self ifFalse: [
		^ UIManager default unhandledErrorDefaultAction: anException
		]