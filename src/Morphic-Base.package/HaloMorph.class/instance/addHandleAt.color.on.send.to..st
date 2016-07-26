private
addHandleAt: aPoint color: aColor on: eventName send: selector to: recipient
	^ self addHandleAt: aPoint color: aColor icon: nil on: eventName send: selector to: recipient
