examples
exampleLazyInit
	^ singleton ifNil: [singleton := self new initialize]