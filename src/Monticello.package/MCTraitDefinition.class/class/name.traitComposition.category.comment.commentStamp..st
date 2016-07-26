instance-creation
name: classNameString traitComposition:  traitCompositionString category:  categoryString comment:  commentString commentStamp:   commentStamp
	^ self instanceLike:
		(self new initializeWithName: classNameString 
			traitComposition:  traitCompositionString
			category:  categoryString
			comment:  commentString  
			commentStamp:   commentStamp)
