geometry
horizontalAdvanceUpTo: aTextPosition layout: aTextLayout
"	<compilerOptions: #(- optionInlineToDo )>"
	| horizontalAdvance fontMetricsProvider   characters spanPosition |

	fontMetricsProvider := self  fontMetricsProvider: aTextLayout. 
	horizontalAdvance := 0.
	characters := self span characters.
	spanPosition := aTextPosition spanPosition.
	
	start to: stop do: [ :index |
		| glyphWidth |
		index > spanPosition  ifTrue: [  ^ horizontalAdvance ].
		glyphWidth := fontMetricsProvider getGlyphWidth: (characters at: index).
		horizontalAdvance := horizontalAdvance + glyphWidth.
	].
	"we should never reach here"
	^ horizontalAdvance