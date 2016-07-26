instance creation
current

	^ Current ifNil: [ Current  := self basicNew initialize ].