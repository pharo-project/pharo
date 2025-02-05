Extension { #name : 'String' }

{ #category : '*AST-Core' }
String >> isValidSelector [
	"check I could be a valid selector (name of method).
	 For checking if there is symbol like me used as a selector, see #isSelectorSymbol on Symbol"

	self deprecated: 'Use OCScanner isSelector: ' 
		on: '02/02/2025'  
		in: 'pharo13' 
		transformWith: '`@rec isValidSelector' -> 'OCScanner isSelector: `@arg'.
	^ OCScanner isSelector: self
]
