spec protocol
openAt: aPoint 

	self widgetDo: [ :w | w invokeAt: (aPoint + (2@7)) in: World allowKeyboard: true ]