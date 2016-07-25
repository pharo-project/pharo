accessing
current
	^ Current ifNil: [ Current := self new ]