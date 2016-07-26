private
askForCategoryIn: aClass default: aString
	| categoryName |
	categoryName := AbstractTool requestProtocolNameFor: aClass initialAnswer: aString.
	categoryName ifNil: [^aString].
	^ categoryName isEmptyOrNil 
		ifTrue: [^ aString] 
		ifFalse: [ categoryName ]