event handling
click
	"Pretend the user clicked on me."

	(self handlesMouseDown: nil) ifTrue: [
		self mouseDown: nil.
		self mouseUp: nil].