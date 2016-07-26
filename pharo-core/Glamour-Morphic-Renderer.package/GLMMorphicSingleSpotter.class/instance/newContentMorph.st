morphic
newContentMorph
	^ (self newColumn: { self newTextMorph. self newListMorph })
		minWidth: 320;
		minHeight: 200;
		yourself