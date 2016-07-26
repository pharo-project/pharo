private
paragraph
	"Paragraph instantiation is lazy -- create it only when needed.
	Don't adjust rightX (as in TexMorph>>paragraph)
	since wrapFlag must be false for layout to work."
	
	paragraph ifNotNil: [^ paragraph].

self setProperty: #CreatingParagraph toValue: true.

	self setDefaultContentsIfNil.

	"...Code here to recreate the paragraph..."
	paragraph := (Paragraph new textOwner: self owner).
	paragraph wantsColumnBreaks: successor notNil.
	paragraph
		compose: text
		style: textStyle copy
		from: self startingIndex
		in: self container.
	"don't adjust the right, let it scroll!"
	paragraph focused: (self currentHand keyboardFocus == self).
	self fit.
self removeProperty: #CreatingParagraph.

	self selectionColor: self selectionColor.
	^ paragraph