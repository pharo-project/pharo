emphasis
fontNumberAt: characterIndex 
	"Answer the fontNumber for characters in the run beginning at characterIndex."
	| attributes fontNumber |
	self size = 0 ifTrue: [^1].	"null text tolerates access"
	attributes := runs at: characterIndex.
	fontNumber := 1.
	attributes do: [:att | (att isMemberOf: TextFontChange) ifTrue: [fontNumber := att fontNumber]].
	^ fontNumber
	