instance creation
string: aString attribute: att
	"Answer an instance of me whose characters are aString.
	att is a TextAttribute."

	^self string: aString attributes: (Array with: att)