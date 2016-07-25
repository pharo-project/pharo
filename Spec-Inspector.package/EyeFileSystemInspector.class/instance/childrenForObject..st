api
childrenForObject: aFileReference
	aFileReference isDirectory ifFalse: [^#()].
	^ aFileReference children collect: [ :each |
		DynamicEyeElement
			host: aFileReference
			value: each
		 ]