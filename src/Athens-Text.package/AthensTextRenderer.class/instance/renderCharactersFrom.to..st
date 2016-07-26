as yet unclassified
renderCharactersFrom: start to: stop

	" accumulate advance while rendering spans"
	"canvas drawShape: (advance x @ advance y extent: 2 @ 2)."
	glyphRenderer advance: advance.
	advance := advance + (glyphRenderer renderCharacters: text from: start to: stop).
	
