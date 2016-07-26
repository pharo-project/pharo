instance creation
openContents: aString
	"Open a new workspace with the given contents. It looks better when aString is code since the style is applied"

	^ self new
		open;
		contents: aString;
		yourself