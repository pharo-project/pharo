examples
example1
	| window scrollPane pasteUpMorph |
	window := SystemWindow new.
	scrollPane := ScrollPane new.
	pasteUpMorph := PasteUpMorph new.
	pasteUpMorph extent: 1000@1000.
	scrollPane scroller addMorph: pasteUpMorph.
	window addMorph: scrollPane frame: (0@0 corner: 1@1).
	window openInWorld.