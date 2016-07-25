initializing
initializeWithName: classNameString 
	traitComposition:  traitCompositionString
	category:  categoryString
	comment:  commentString  
	commentStamp:   commentStampString
					
		name := classNameString asSymbol.
		traitComposition := traitCompositionString.
	     category := categoryString.
		comment := commentString withSqueakLineEndings.
		commentStamp :=  commentStampString ifNil: [self defaultCommentStamp]
