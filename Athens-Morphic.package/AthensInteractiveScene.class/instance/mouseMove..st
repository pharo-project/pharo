event handling
mouseMove: evt

	((self inState: #panning) or: [ self inState:#zooming ]) 
		ifFalse: [
			(self eventHandledByScene: evt) ifTrue: [ ^ self ]. 	
		].
	^ super mouseMove: evt