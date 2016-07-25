rendering
renderCharacters: aString from: start to: stop 
	

	" accumulate advance while rendering spans"
	"canvas drawShape: (advance x @ advance y extent: 2 @ 2)."
	canvas 
		setPaint: color;
		setFont: font;
		drawString: aString from: start to: stop