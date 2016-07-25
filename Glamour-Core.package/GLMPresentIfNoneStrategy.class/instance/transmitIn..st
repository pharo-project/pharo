transmitting
transmitIn: aContext
	(self transmission destination hasPane and: [
		self transmission destination pane presentations isEmpty]) ifTrue: [	
			self transmission destination pane
				notingPresentationChangeDo: [
					self transmission destination pane clearIn: aContext.
					self transmission destination pane presentations: self presentations copy ] ]