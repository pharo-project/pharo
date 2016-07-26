instance creation
newFontFamilyMorph
	"Answer a list for the font family of the font."

	|highestFont|
	highestFont := self fontFamilies first fontAt: 1 withStyle: TextStyle default.
	self fontFamilies do: [:ff | |f|
		f := ff fontAt: 1 withStyle: TextStyle default.
		f height > highestFont height
			ifTrue: [highestFont := f]].
	^(self
		newListFor: self
		list: #fontFamilies
		selected: #fontFamilyIndex
		changeSelected: #fontFamilyIndex:
		help: nil)
		font: highestFont