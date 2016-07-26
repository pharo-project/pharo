protocol
remove: oldObject

	| result |
	result := value remove: oldObject.
	self valueRemoved: oldObject.
	^ result