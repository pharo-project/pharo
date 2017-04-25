accessing
hasChildrenBlock 
	^ self parameters at: 'hasChildrenBlock' ifAbsent: [ nil ]