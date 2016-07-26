private
hasMinHeight

	^ (self minHeight isNumber ifTrue: [ self minHeight <= 0 ] ifFalse: [ false ]) not