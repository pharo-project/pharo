private
hasMaxHeight

	^ (self maxHeight isNumber ifTrue: [ self maxHeight isInfinite ] ifFalse: [ false ]) not