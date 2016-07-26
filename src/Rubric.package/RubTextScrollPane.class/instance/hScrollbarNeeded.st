scrollbar managing
hScrollbarNeeded
	"Return whether the horizontal scrollbar is needed."
	self wrapped ifTrue: [  ^ false ].
	^ super hScrollbarNeeded