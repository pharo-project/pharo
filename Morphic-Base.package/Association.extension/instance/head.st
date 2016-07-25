*Morphic-Base
head

	^ (key isKindOf: Association)
		ifTrue: [ key head ]
		ifFalse: [ key ]