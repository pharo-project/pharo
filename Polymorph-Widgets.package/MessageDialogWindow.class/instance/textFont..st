accessing
textFont: aFont
	"Set the text font."
	
	textFont :=  aFont.
	self textMorph ifNotNil: [:m | m font: aFont]