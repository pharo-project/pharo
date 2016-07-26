private
addGraphicalHandle: formKey at: aPoint on: eventName send: selector to: recipient
	"Add the supplied form as a graphical handle centered at the given point, and set it up to respond to the given event by sending the given selector to the given recipient.  Return the handle."
	| handle |
	handle := self addGraphicalHandleFrom: formKey at: aPoint.
	handle on: eventName send: selector to: recipient.
	handle setBalloonText: (target balloonHelpTextForHandle: handle) translated.
	^ handle
