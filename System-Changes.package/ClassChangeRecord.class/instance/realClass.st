isolation layers
realClass
	"Return the actual class (or meta), as determined from my name."

	thisName ifNil: [ ^ nil ].
	(thisName endsWith: ' class')
		ifTrue: [ ^ (Smalltalk globals at: (thisName copyFrom: 1 to: thisName size - 6) asSymbol ifAbsent: [ ^ nil ]) class ]
		ifFalse: [ ^ Smalltalk globals at: thisName ifAbsent: [ ^ nil ] ]