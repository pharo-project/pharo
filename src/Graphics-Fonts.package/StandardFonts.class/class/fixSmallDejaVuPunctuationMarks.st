small font fixing
fixSmallDejaVuPunctuationMarks
	"Just a small hack to make small font punctuation marks readable"
	| fb |
	StandardFonts setFontsToStyleNamed: #small. "To be sure we use the small font"
	fb :=StandardFonts defaultFont emphasized: 1. "copy the bold font"
	"For each punctuation mark, replace the corresponding glyph with the one found in the bold font" 
	#($. $: $; $' $` $, $")
		do: [:ch |
			StandardFonts defaultFont characterFormAt: ch put: (fb characterFormAt: ch) deepCopy].