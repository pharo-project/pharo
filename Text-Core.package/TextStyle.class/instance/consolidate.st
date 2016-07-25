private
consolidate
	"If this style includes any fonts that are also in the default style,
	then replace them with references to the default ones."
	"
	TextStyle allInstancesDo: [:s | s == TextStyle default ifFalse: [s consolidate]]
"
	| defFonts font |
	defFonts := TextStyle default fontArray.
	1 
		to: fontArray size
		do: 
			[ :i | 
			font := fontArray at: i.
			1 
				to: defFonts size
				do: 
					[ :j | 
					(font name asUppercase copyWithout: $ ) = ((defFonts at: j) name asUppercase copyWithout: $ ) ifTrue: 
						[ fontArray 
							at: i
							put: (defFonts at: j) ] ] ]