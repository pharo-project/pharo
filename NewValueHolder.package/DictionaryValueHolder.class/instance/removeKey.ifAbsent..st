protocol
removeKey: key ifAbsent: aBlock 
	| result |
	
	result := value removeKey: key ifAbsent: aBlock.
	self valueChanged.
	^ result