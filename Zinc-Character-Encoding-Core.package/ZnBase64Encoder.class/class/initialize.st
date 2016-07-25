class initialization
initialize
	DefaultAlphabet := String withAll: ($A to: $Z) , ($a to: $z) , ($0 to: $9) , #($+ $/).
	DefaultInverse := Array new: 128.
	0 to: 127 do: [ :each | 
		| offset |
		offset := DefaultAlphabet indexOf: each asCharacter ifAbsent: [ nil ].
		DefaultInverse at: each + 1 put: (offset ifNotNil: [ offset - 1 ]) ]