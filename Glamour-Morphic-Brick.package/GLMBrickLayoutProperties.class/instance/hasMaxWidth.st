private
hasMaxWidth

	^ (self maxWidth isNumber ifTrue: [ self maxWidth isInfinite ] ifFalse: [ false ]) not