"
I'm a dialog to select a font. 

Usage example:

|fontSelector|
fontSelector := FreeTypeFontSelectorDialogWindow new.
UITheme builder openModal: fontSelector.
fontSelector selectedFont inspect.
"
Class {
	#name : #FreeTypeFontSelectorDialogWindow,
	#superclass : #AbstractFontSelectorDialogWindow,
	#category : 'FontChooser'
}

{ #category : #'as yet unclassified' }
FreeTypeFontSelectorDialogWindow >> defaultFontFamilies [
	"Answer the set of available fonts families that are supported in the font that they represent."
	
	|fonts defaultFont|
	defaultFont := TextStyle default fontOfPointSize: self theme listFont pointSize.
	fonts := (LogicalFontManager current allFamilies asSortedCollection: [:a :b | 
		a familyName <= b familyName]) collect: [:ff |
			 (ff
				closestMemberWithStretchValue: LogicalFont stretchRegular
				weightValue: LogicalFont weightRegular
				slantValue: LogicalFont slantRegular)
				asLogicalFontOfPointSize: self theme listFont pointSize].
	^fonts collect: [:f | |dispFont|
		dispFont := defaultFont.
		f familyName asText
			addAttribute: (TextFontReference toFont: dispFont)]
]

{ #category : #'as yet unclassified' }
FreeTypeFontSelectorDialogWindow >> matchingFont [
	"Answer the font that matches the selections."

	|lf emp|
	self familyName ifNil: [^TextStyle defaultFont].
	lf := LogicalFont
		familyName: self familyName 
		pointSize: (self fontSize ifNil: [10]).
	emp := self isBold
		ifTrue: [TextEmphasis bold emphasisCode]
		ifFalse: [TextEmphasis normal emphasisCode].
	self isItalic
		ifTrue: [emp := emp + TextEmphasis italic emphasisCode].
	self isUnderlined
		ifTrue: [emp := emp + TextEmphasis underlined emphasisCode].
	self isStruckOut
		ifTrue: [emp := emp + TextEmphasis struckOut emphasisCode].
	lf := lf emphasis: emp.
	lf realFont ifNil: [^TextStyle defaultFont].
	^lf 
]

{ #category : #'instance creation' }
FreeTypeFontSelectorDialogWindow >> newFontStyleButtonRowMorph [
	"Answer a new font style button row morph."

	^self newRow: {
		self newBoldButtonMorph.
		self newItalicButtonMorph}
]

{ #category : #'as yet unclassified' }
FreeTypeFontSelectorDialogWindow >> updateFromSelectedFont [
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
]
