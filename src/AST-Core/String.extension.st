Extension { #name : #String }

{ #category : #'*AST-Core' }
String >> isValidSelector [
	"check I could be a valid selector (name of method).
	 For checking if there is symbol like me used as a selector, see #isSelectorSymbol on Symbol"
	^ RBScanner isSelector: self
]
