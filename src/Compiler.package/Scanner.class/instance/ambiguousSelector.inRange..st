error handling
ambiguousSelector: aString inRange: anInterval
	"Warning: this methods breaks backward compatibility: $- is now part of literal argument.."
	
	token := aString asSymbol.
	^self