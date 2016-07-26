*Morphic-Base
on: eventName send: selector to: recipient
	self eventHandler ifNil: [self eventHandler: MorphicEventHandler new].
	self eventHandler on: eventName send: selector to: recipient