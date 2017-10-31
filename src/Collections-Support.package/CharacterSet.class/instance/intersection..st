enumerating
intersection: anotherSet
	| smaller larger |
	(anotherSet abstractSize = Size small and: [ self size < anotherSet size ])
		ifTrue: [ smaller := self.
			larger := anotherSet ]
		ifFalse: [ smaller := anotherSet.
			larger := self ].
	^ smaller intersectSmallWithLarge: larger