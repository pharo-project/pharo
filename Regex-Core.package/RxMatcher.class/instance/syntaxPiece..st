double dispatch
syntaxPiece: pieceNode
	"Double dispatch from the syntax tree. 
	Piece is an atom repeated a few times. Take care of a special
	case when the atom is repeated just once."

	| atom |
	atom := pieceNode atom dispatchTo: self.
	^pieceNode isSingular
		ifTrue: [atom]
		ifFalse: [pieceNode isStar
			ifTrue: [self makeStar: atom]
			ifFalse: [pieceNode isPlus
				ifTrue: [self makePlus: atom]
				ifFalse: [pieceNode isOptional
					ifTrue: [self makeOptional: atom]
					ifFalse: [self makeQuantified: atom min: pieceNode min max: pieceNode max]]]]