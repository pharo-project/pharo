*Morphic-Base
tail

	^ (key isKindOf: Association)
		ifTrue: [ key tail -> value ]
		ifFalse: [ value ]