emphasis
fontAt: characterIndex withStyle: aTextStyle
	"Answer the fontfor characters in the run beginning at characterIndex."
	| attributes font |
	self size = 0 ifTrue: [^ aTextStyle defaultFont].	"null text tolerates access"
	attributes := runs at: characterIndex.
	font := aTextStyle defaultFont.  "default"
	attributes do: 
		[:att | att forFontInStyle: aTextStyle do: [:f | font := f]].
	^ font