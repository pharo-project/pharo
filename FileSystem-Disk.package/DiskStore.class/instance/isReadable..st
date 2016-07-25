public
isReadable: aPath
	(self exists: aPath) ifFalse: [ ^ false ].
	self flag: 'TODO: we need a decent primitive for this...'.
	(self basicOpen: aPath writable: false) 
		ifNotNil: [ :id|
			Primitives close: id.
			^ true].
	^ false