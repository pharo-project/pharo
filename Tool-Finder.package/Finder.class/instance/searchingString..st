accessing
searchingString: aString
	"When the search string is modified (in fact, when I make a search) I rebuild the result dictionary"

	searchingString := aString.
	self constructDictionary.