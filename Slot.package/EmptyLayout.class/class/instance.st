instance creation
instance
	^ instance ifNil: [ instance := self new ]