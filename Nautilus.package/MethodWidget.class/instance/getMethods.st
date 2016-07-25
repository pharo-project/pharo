protocol
getMethods

	^ methods ifNil: [ methods := self loadMethods ].