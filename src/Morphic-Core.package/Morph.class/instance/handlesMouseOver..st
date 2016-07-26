event handling
handlesMouseOver: evt
	"Do I want to receive mouseEnter: and mouseLeave: when the button is up and the hand is empty?  The default response is false, except if you have added sensitivity to mouseEnter: or mouseLeave:, using the on:send:to: mechanism." 

	self eventHandler ifNotNil: [^ self eventHandler handlesMouseOver: evt].
	^ false