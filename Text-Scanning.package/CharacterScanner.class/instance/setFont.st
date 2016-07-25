private
setFont
	| priorFont |
	"Set the font and other emphasis."
	priorFont := font.
	text ifNotNil:[
		emphasisCode := 0.
		kern := 0.
		indentationLevel := 0.
		alignment := textStyle alignment.
		font := nil.
		(text attributesAt: lastIndex forStyle: textStyle)
			do: [:att | att emphasizeScanner: self]].
	font ifNil: [self setFont: textStyle defaultFontIndex].
	self setActualFont: (font emphasized: emphasisCode).
	priorFont 
		ifNotNil: [
			font = priorFont 
				ifTrue:[
					"font is the same, perhaps the color has changed?
					We still want kerning between chars of the same
					font, but of different color. So add any pending kern to destX"
					destX := destX + (pendingKernX ifNil:[0])].
			destX := destX + priorFont descentKern].
	pendingKernX := 0. "clear any pending kern so there is no danger of it being added twice"
	destX := destX - font descentKern.
	"NOTE: next statement should be removed when clipping works"
	leftMargin ifNotNil: [destX := destX max: leftMargin].
	kern := kern - font baseKern.

	"Install various parameters from the font."
	spaceWidth := font widthOf: Space.