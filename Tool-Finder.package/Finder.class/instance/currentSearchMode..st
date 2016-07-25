accessing
currentSearchMode: aSymbol
	"Setter"
	"If #Examples is selected, I disable the RegEx checkbo. Then I rebuild the resultDictionary"

	currentSearchMode := aSymbol.
	self isExamplesSymbol
		ifTrue: [ self disableUseRegEx]
		ifFalse:[ self enableUseRegEx].
		
	self update: #sourceCode.
	self constructDictionary