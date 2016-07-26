accessing
peekFor: anElement
	^ self peek = anElement and: [ self next. true ]