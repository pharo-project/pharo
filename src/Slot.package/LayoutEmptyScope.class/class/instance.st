accessing
instance
	^ instance ifNil: [ instance := self new ]