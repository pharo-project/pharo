examples
example2
	| window scrollPane pasteUpMorph point textMorph |
	window := SystemWindow new.
	scrollPane := ScrollPane new.
	pasteUpMorph := PasteUpMorph new.
	pasteUpMorph extent: 1000@1000.
	scrollPane scroller addMorph: pasteUpMorph.
	window addMorph: scrollPane frame: (0@0 corner: 1@1).
	0 to: 1000 by: 100 do: 
		[:x | 0 to: 1000 by: 100 do:
			[:y |
				point :=  x@y.
				textMorph := TextMorph new contents: point asString.
				textMorph position: point.
				pasteUpMorph addMorph: textMorph
			]
		].
	window openInWorld.