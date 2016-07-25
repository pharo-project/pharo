accessing
changeClass
	| class |
	class := Smalltalk globals at: self changeClassName ifAbsent: [ ^ nil ].
	^ isMeta ifTrue: [ class class ] ifFalse: [ class ]