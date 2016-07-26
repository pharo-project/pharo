drawing text
drawString: aString
	"Draw a string using currently selected font.
	Answer a total advance of rendered string "
	^ fontRenderer renderCharacters: aString from: 1 to: aString size.