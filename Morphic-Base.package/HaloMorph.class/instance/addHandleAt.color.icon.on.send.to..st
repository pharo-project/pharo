private
addHandleAt: aPoint color: aColor icon: iconName on: eventName send: selector to: recipient 
	"Add a handle centered at the given point with the given color, 
	and set it up to respond to the given event by sending the 
	given selector to the given recipient. Return the handle."
	| handle |
	handle := self createHandleAt: aPoint color: aColor iconName: iconName.
	self addMorph: handle.

	handle on: #mouseUp send: #endInteraction to: self.
	handle on: eventName send: selector to: recipient.
	handle setBalloonText: (target balloonHelpTextForHandle: handle) translated.

	^ handle 