as yet unclassified
updateFromSelectedFont
	"Update our state based on the selected font."

	|font|
	font := self selectedFont ifNil: [TextStyle defaultFont].
	fontFamilyIndex := (self fontFamilies indexOf: font familyName).
	fontSizeIndex := (self fontSizes indexOf: font pointSize).
	isBold := (font emphasis allMask: TextEmphasis bold emphasisCode).
	isItalic := (font emphasis allMask: TextEmphasis italic emphasisCode).
	self
		changed: #fontFamilyIndex;
		changed: #fontSizeIndex;
		changed: #isBold;
		changed: #isItalic.
	self textPreviewMorph ifNotNil: [:tp |
		tp font: self selectedFont.
		self changed: #previewText]