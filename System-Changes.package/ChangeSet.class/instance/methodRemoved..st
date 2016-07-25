change logging
methodRemoved: anEvent
	self
		removeSelector: anEvent selector
		class: anEvent methodClass
		priorMethod: anEvent method
		lastMethodInfo: {anEvent method sourcePointer. anEvent protocol}