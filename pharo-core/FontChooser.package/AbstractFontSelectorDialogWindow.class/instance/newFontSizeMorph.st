instance creation
newFontSizeMorph
	"Answer a list for the font size of the font."

	^self
		newListFor: self
		list: #fontSizes
		selected: #fontSizeIndex
		changeSelected: #fontSizeIndex:
		help: nil