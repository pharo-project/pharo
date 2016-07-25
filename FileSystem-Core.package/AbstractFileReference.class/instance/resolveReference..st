navigating
resolveReference: aReference
	^ aReference isAbsolute
		ifTrue: [ aReference ]
		ifFalse: [ self withPath: aReference path ]