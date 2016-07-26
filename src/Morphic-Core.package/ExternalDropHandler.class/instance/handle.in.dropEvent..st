accessing
handle: dropStream in: pasteUp dropEvent: anEvent
	
	^action cull: dropStream 
			cull: pasteUp 
			cull: anEvent
	