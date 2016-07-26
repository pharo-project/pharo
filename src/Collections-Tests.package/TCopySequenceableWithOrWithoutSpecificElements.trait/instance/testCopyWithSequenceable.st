tests - copying with or without
testCopyWithSequenceable

	| result index element |
	index := self indexInNonEmpty .
	element := self nonEmpty at: index.
	result := self nonEmpty copyWith: (element ).
	
	self assert: result size = (self nonEmpty size + 1).
	self assert: result last = element .
	
	1 to: (result size - 1) do:
	[ :i |
	self assert: (result at: i) = ( self nonEmpty at: ( i  ))].