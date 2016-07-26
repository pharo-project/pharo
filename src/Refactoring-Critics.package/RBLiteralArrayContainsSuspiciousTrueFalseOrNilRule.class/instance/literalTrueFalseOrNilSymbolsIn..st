private
literalTrueFalseOrNilSymbolsIn: aLiteral 
	| retval |
	aLiteral class == Array ifFalse: [ ^ #() ].
	retval := OrderedCollection withAll: (aLiteral 
		select: [ :each | each isSymbol and: [ #(#true #false #nil ) includes: each ] ]).
	aLiteral do: [ :each | retval addAll: (self literalTrueFalseOrNilSymbolsIn: each) ].
	^ retval