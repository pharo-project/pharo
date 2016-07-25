running
basicCheck: aClass
	| count current |
	aClass isMeta ifTrue: [ ^ false ].
	count := 1.
	current := aClass.
	[ current isNil ] whileFalse: [
		self inheritanceDepth < count
			ifTrue: [ ^ true ].
		current := current superclass.
		count := count + 1 ].
	
	^ false