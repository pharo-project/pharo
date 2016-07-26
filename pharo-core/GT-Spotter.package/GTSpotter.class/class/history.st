accessing
history
	^ history ifNil: [ history := WeakOrderedCollection new ]