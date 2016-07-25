ui-building
newContentMorph
	"Answer a new content morph."

	| sp choices |
	self choicesMorph: (choices := self newChoicesMorph).
	sp := (self newScrollPaneFor: choices)
		color: Color transparent;
		scrollTarget: choices;
		hResizing: #spaceFill;
		vResizing: #spaceFill.
	sp
		minWidth: (scrollPaneWidth ifNil: [(choicesMorph width min: (Display width // 2) - 50) + sp scrollBarThickness]);
		minHeight: (choicesMorph height min: (Display height // 3)).
	choicesMorph width > sp minWidth
		ifTrue: [sp minHeight: sp minHeight + sp scrollBarThickness].
	^ sp