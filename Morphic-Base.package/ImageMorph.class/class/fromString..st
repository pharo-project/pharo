instance creation
fromString: aString 
	"Create a new ImageMorph which displays the input string in the standard button font"

	^ self fromString: aString font: StandardFonts buttonFont