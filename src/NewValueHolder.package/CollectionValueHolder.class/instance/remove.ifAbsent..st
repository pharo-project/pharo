protocol
remove: oldObject ifAbsent: absentBlock

	| result |
	result := value remove: oldObject ifAbsent: absentBlock.
	self valueRemoved: oldObject.
	^ result