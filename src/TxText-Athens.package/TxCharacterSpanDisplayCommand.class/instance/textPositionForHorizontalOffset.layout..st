geometry
textPositionForHorizontalOffset: anOffset layout: layout
	| x fontMetricsProvider  |

	fontMetricsProvider := self fontMetricsProvider: layout.
	x := anOffset.

	start to: stop do: [ :index |
		| glyphWidth |	
		glyphWidth := fontMetricsProvider getGlyphWidth: (span characters at: index).
		x <= glyphWidth ifTrue: [ 
			"if offset closer to left side of glyph, we answer the position prior to it,
			otherwise after it"
			x < (glyphWidth/2) 
				ifTrue: [ ^ span positionAt: index - 1 ]
				ifFalse: [ ^ span positionAt: index ]
			].
		x := x - glyphWidth].
	^ span positionAt: stop