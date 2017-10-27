initialization
initialize

	super initialize.
	displayBlockHolder := [ :object | object ifNotNil: [ object asStringOrText ] ifNil: '' ] asValueHolder.
	displayBlockHolder whenChangedDo: [ self date: self date ].
	self date: self defaultDate.