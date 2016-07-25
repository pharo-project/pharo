event-handling
diveInto: anObject
	self owner ifNotNil: [ :navigator | 
		(navigator respondsTo: #diveInto:)
			ifTrue: [ ^ navigator diveInto: anObject ] ].
		
	self inspectInNewWindow: anObject.