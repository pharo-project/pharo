enquiries
methodDescriptionForSelector: aSymbol
	"Return a TraitMethodDescription for the selector aSymbol."

	| description |
	description := TraitMethodDescription selector: aSymbol.
	self transformations do: [:each |
		each collectMethodsFor: aSymbol into: description].
	^description