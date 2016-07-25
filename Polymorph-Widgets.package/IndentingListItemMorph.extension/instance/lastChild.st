*Polymorph-Widgets
lastChild
	"Answer the last child."

	|c|
	c := self firstChild ifNil: [^nil].
	[c nextSibling isNil] whileFalse: [c := c nextSibling].
	^c