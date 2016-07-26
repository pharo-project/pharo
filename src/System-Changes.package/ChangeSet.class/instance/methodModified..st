change logging
methodModified: anEvent
	self
		noteNewMethod: anEvent newMethod
		forClass: anEvent methodClass
		selector: anEvent selector
		priorMethod: anEvent oldMethod