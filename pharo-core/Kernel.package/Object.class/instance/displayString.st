displaying
displayString
	"While printString is about to give a detailled information about an object, displayString is a message that should return a short string-based representation to be used by list and related UI frameworks. By default, simply return printString."
	"asString should not be implemented in Object, and kept for conversion between strings, symbols, text and characters."
	
	^ self printString
