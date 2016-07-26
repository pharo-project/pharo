accessing
changeClass
	| class |
	class := Smalltalk globals at: self changeClassName ifAbsent: [ ^ nil ].
	^ isMeta ifTrue: [ class classSide ] ifFalse: [ class ]