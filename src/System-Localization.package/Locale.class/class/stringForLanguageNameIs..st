accessing
stringForLanguageNameIs: localeID 
	"Answer a string for a menu determining whether the given  
	symbol is the project's natural language"
	^ (self current localeID = localeID
		ifTrue: ['<yes>']
		ifFalse: ['<no>'])
		, localeID displayLanguage