event
mouseUp: evt
	"Handle the transitions."
	| result |
	result := super mouseUp: evt.
	self pressed: index.
	^ result 