events-processing
handlesMouseMove: anEvent
	"Rules say that by default a morph gets #mouseMove iff
		* the hand is not dragging anything,
			+ and some button is down,
			+ and the receiver is the current mouse focus."
			
	"self eventHandler ifNotNil: [:handler | ^handler handlesMouseMove: anEvent ]."
				
	^anEvent hand submorphs isEmpty and: [ 	 
		(anEvent anyButtonPressed and:[
			anEvent hand mouseFocus == self
		])]
	
