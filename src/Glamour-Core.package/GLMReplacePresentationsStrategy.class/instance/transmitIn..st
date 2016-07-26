transmitting
transmitIn: aContext
	"it is important for the destination to be set after the presentation, 
	because when the presentation is a browser, we might forward the entity internally"
	self transmission destination hasPane ifTrue: [
		self transmission destination pane notingPresentationChangeDo: [
			self transmission destination pane clearIn: aContext.
			self transmission destination pane presentations: self presentations copy ]].
