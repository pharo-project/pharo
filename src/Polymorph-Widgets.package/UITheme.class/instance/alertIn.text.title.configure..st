services
alertIn: aThemedMorph text: aStringOrText title: aString configure: aBlock
	"Answer the result of an alert dialog (true) with the given label and title."

	|dialog|
	SoundTheme useThemeSounds ifTrue: [self alertSound play].
	dialog := (AlertDialogWindow newWithTheme: aThemedMorph theme)
		textFont: self textFont;
		title: aString;
		text: aStringOrText.
	aBlock value: dialog.
	aThemedMorph openModal: dialog.
	^dialog cancelled not