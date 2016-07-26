accessing methods
includesSelector: selector
	"Looks if selector is a key in the methods dictionary"
	
	^methods includesKey: selector asSymbol