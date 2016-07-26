styling
emphasizeScanner: scanner
	"Set the emphasis for text display"
	"self backgroundColor ifNotNil: [ :bc | scanner backgroundColor: bc ]."
	self textColor ifNotNil: [:c | scanner textColor: c].
	emphasisCode ifNotNil: [ scanner addEmphasis: emphasisCode ]