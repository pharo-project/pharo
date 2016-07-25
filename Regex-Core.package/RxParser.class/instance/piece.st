recursive descent
piece
	"<piece> ::= <atom> | <atom>* | <atom>+ | <atom>? | <atom>{<number>,<number>}"

	| atom |
	atom := self atom.
	
	lookahead = $*
		ifTrue: [ 
			self next.
			atom isNullable
				ifTrue: [ self signalNullableClosureParserError ].
			^ RxsPiece new initializeStarAtom: atom ].

	lookahead = $+
		ifTrue: [ 
			self next.
			atom isNullable
				ifTrue: [ self signalNullableClosureParserError ].
			^ RxsPiece new initializePlusAtom: atom ].

	lookahead = $?
		ifTrue: [ 
			self next.
			atom isNullable
				ifTrue: [ 
					^ self lookAround ].
			^ RxsPiece new initializeOptionalAtom: atom ].
	
	lookahead = ${
		ifTrue: [
			^ self quantifiedAtom: atom ].
		
	^ RxsPiece new initializeAtom: atom