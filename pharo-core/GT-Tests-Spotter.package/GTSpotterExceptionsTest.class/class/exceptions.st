accessing
exceptions
	^ exceptions ifNil: [ exceptions := OrderedCollection new ]