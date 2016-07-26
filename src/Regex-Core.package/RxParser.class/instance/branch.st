recursive descent
branch
	"<branch> ::= e | <piece> <branch>"

	| piece branch |
	piece := self piece.
	(lookahead = #epsilon 
	or: [ lookahead = $| 
	or: [ lookahead = $) ]])
		ifTrue: [ branch := nil ]
		ifFalse: [ branch := self branch ].
	^RxsBranch new 
		initializePiece: piece 
		branch: branch