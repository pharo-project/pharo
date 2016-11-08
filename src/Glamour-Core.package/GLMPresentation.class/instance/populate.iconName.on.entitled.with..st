scripting actions
populate: aPortSymbol iconName: aSymbol on: aCharacter entitled: aString with: aBlock 
	 
	self populate: aPortSymbol icon: (self iconNamed: aSymbol) on: aCharacter entitled: aString with: aBlock 