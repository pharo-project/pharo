events-processing
handleMouseMove: anEvent

	(anEvent wasHandled or: [ resetable not ]) ifTrue:[ ^self ]. 
	
	self resetAlpha.

	anEvent wasHandled: true.
