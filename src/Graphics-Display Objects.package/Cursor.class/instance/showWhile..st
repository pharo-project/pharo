displaying
showWhile: aBlock 
	"While evaluating the argument, aBlock, make the receiver be the cursor shape."
	| oldcursor |
	oldcursor := self class currentCursor.
	self show.
	^aBlock ensure: [oldcursor show]
