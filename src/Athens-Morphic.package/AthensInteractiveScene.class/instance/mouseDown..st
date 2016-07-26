event handling
mouseDown: evt

	((self inState: #panning) or: [ self inState:#zooming ]) 
		ifFalse: [
			(self eventHandledByScene: evt) ifTrue: [ ^ self ]. 	
		].

	^ super mouseDown: evt