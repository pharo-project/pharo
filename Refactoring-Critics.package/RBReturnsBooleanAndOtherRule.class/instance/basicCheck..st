running
basicCheck: aMethod 
	| hasBool hasSelf |
	hasBool := false.
	hasSelf := aMethod parseTree lastIsReturn not.
	(matcher executeTree: aMethod parseTree initialAnswer: Set new) do: [ :each | 
		hasBool := hasBool 
			or: [ (each isLiteralNode and: [ #(true false) includes: each value ]) 
			or: [ (each isMessage and: [ #(and: or:) includes: each selector ]) ] ].
		hasSelf := hasSelf
			or: [ (each isVariable and: [ each name = 'self' ]) 
			or: [ (each isLiteralNode and: [ (#(true false) includes: each value) not ]) ] ] ].
	^ (hasSelf and: [ hasBool ])