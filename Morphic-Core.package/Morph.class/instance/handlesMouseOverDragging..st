event handling
handlesMouseOverDragging: evt
	"Return true if I want to receive mouseEnterDragging: and mouseLeaveDragging: when the hand drags something over me (button up or button down), or when the mouse button is down but there is no mouseDown recipient.  The default response is false, except if you have added sensitivity to mouseEnterLaden: or mouseLeaveLaden:, using the on:send:to: mechanism."
	"NOTE:  If the hand state matters in these cases, it may be tested by constructs such as
		event anyButtonPressed
		event hand hasSubmorphs"

	self eventHandler ifNotNil: [^ self eventHandler handlesMouseOverDragging: evt].
	^ false