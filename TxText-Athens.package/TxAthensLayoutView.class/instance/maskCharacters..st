internal-masking
maskCharacters: characters
	"This is a hook to possibly replace original characters , which _always_ coming from spans during scanning the text
	with something else. This method should answer a string with exactly same number of characters as in provided one.
	Default implementation just answers original characters."
	
	^ characters 