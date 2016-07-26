event handling
mouseUp: evt
	"Allow on:send:to: to set the response to events other than actWhen"
	actWhen == #buttonUp ifFalse: [^super mouseUp: evt].

	(self containsPoint: evt cursorPoint) ifTrue: [
		self state: #on.
		self doButtonAction: evt
	] ifFalse: [
		self state: #off.
	].
	"Allow owner to keep it selected for radio buttons"
