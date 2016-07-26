*Morphic-Base
on: eventName send: selector to: recipient withValue: value
	"NOTE: selector must take 3 arguments, of which value will be the *** FIRST ***"

	self eventHandler ifNil: [self eventHandler: MorphicEventHandler new].
	self eventHandler on: eventName send: selector to: recipient withValue: value
