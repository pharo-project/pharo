accessing
bindingOf: varName
	^ self associationAt: varName ifAbsent: [nil]