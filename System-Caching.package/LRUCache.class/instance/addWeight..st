private
addWeight: value 
	weight add: value.
	[ weight isBelowMaximum ]
		whileFalse: [ 
			self isEmpty
				ifTrue: [ self error: 'Weight of single value being added exceeds maximum' ]
				ifFalse: [ self evict ] ]