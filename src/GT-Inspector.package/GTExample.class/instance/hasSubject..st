testing
hasSubject: aSubject
	^ self classNamed: aSubject do: [ :class | self subjects includes: class ] ifNone: [ false ]