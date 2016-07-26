drawing text
drawString: aString from: start to: end

	"Draw a portion of string using currently selected font.
	Answer a total advance of rendered portion"
	^ fontRenderer renderCharacters: aString from: start to: end