testing
includesBehaviorNamed: aClassName
	| current |
	current := self selectedClass.
	[ current isNil ] whileFalse: [
		current name = aClassName
			ifTrue: [ ^ true ].
		current := current superclass ].
	^ false