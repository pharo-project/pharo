flagging
isThisEverCalled: msg
	"Send this message, with some useful printable argument, from methods or branches of methods which you believe are never reached."

	self error: 'This is indeed called: ', msg printString