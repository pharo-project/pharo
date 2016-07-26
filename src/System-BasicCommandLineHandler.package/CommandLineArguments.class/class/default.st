instance creation
default
	^ singleton ifNil: [singleton := self new initialize]