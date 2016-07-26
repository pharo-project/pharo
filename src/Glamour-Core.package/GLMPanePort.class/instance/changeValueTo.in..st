accessing
changeValueTo: anObject in: aTransmissionContext
	| oldValue | 
	oldValue := self value.
	self pane notingPresentationChangeDo: [
		self silentValue: anObject.  
		self pane portEvent: 
					(GLMPortEvent 
						on: self
						previouslyValued: oldValue
						in: aTransmissionContext) ]