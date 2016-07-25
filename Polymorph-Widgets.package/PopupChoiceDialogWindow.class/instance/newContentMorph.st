as yet unclassified
newContentMorph
	"Answer a new content morph."

	|sp choices|
	self choicesMorph: (choices := self newChoicesMorph).
	sp := (self newScrollPaneFor: choices)
		scrollTarget: choices;
		hResizing: #spaceFill;
		vResizing: #spaceFill.
	sp
		minWidth: ((choices width min: Display width // 2) + sp scrollBarThickness max: TextEntryDialogWindow minimumWidth);
		minHeight: (choices height min: Display height // 2).
	choices width > sp minWidth
		ifTrue: [sp minHeight: sp minHeight + sp scrollBarThickness].
	sp
		updateScrollbars.
	^self newGroupboxFor: sp