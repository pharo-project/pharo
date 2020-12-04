Extension { #name : #FreeTypeFont }

{ #category : #'*FreeType-Morphic' }
FreeTypeFont >> gtInspectorFontIn: composite [
	<gtInspectorPresentationOrder: 0>
	composite morph
		title: 'Font';
		display: [ :x | | pane |
			pane := ScrollPane new
						alwaysShowScrollBars: true;
						yourself.
			pane scroller addMorph:
					((String streamContents: [ :s | 22 to: 255 do: [ :idx | s nextPut: idx asCharacter ] ]) asMorph
						font: x;
						yourself).
			pane ]
]
