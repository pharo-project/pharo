initialization
initialize

	displayBlockHolder := [ :object | object asStringOrText ] asValueHolder.
	displayBlockHolder whenChangedDo: [ self date: self date ].
	super initialize. "do this last so default will be set after displayBlock"