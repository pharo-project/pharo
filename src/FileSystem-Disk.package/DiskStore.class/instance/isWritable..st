public
isWritable: aPath
	(self exists: aPath) ifFalse: [ ^ false ].
	self flag: 'TODO: we need a decent primitive for this...'.
	(self basicOpen: aPath writable: true) 
		ifNotNil: [ :id|
			Primitives close: id.
			^ true].
	^ false