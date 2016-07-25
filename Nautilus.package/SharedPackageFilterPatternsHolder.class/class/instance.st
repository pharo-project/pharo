accessing
instance

	^ instance ifNil: [ instance := self new value: OrderedCollection new; yourself ]