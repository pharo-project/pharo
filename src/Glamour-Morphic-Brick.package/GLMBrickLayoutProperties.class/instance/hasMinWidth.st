private
hasMinWidth

	^ (self minWidth isNumber ifTrue: [ self minWidth <= 0 ] ifFalse: [ false ]) not