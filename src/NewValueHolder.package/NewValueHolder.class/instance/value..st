accessing
value: anObject
	
	self handleCircularReferencesWhile: [ | oldValue |
		oldValue := value.
		value := anObject.
		self valueChanged: oldValue ].