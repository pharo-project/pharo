event handling
handlesMouseDown: evt
	"Do I want to receive mouseDown events (mouseDown:, mouseMove:, mouseUp:)?"
	"NOTE: The default response is false, except if you have added sensitivity to mouseDown events using the on:send:to: mechanism.  Subclasses that implement these messages directly should override this one to return true." 

	self eventHandler ifNotNil: [^ self eventHandler handlesMouseDown: evt].
	^ false