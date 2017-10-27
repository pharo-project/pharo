menu messages
selectedSymbol
	"Return the currently selected symbol, or nil if none.  Spaces, tabs and returns are ignored"

	| aString |
	self hasCursor ifTrue: [^ nil].
	aString := self selection string copyWithoutAll:
		{Character space.  Character cr.  Character tab}.
	aString isEmpty ifTrue: [^ nil].
	Symbol hasInterned: aString  ifTrue: [:sym | ^ sym].

	^ nil