event handling
mouseUp: evt

	((self inState: #panning) or: [ self inState:#zooming ]) 
		ifFalse: [
			(self eventHandledByScene: evt) ifTrue: [ ^ self ]. 	
		].
		
	^ super mouseUp:evt 