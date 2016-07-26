private
paragraph
	"Paragraph instantiation is lazy -- create it only when needed"
	|newParagraph|
	paragraph ifNotNil: [^ paragraph].

	self setProperty: #CreatingParagraph toValue: true.
	self setDefaultContentsIfNil.

	"...Code here to recreate the paragraph..."
	newParagraph  := (Paragraph new textOwner: self owner).
	
	newParagraph wantsColumnBreaks: successor notNil.
	newParagraph
		compose: text
		style: textStyle copy
		from: self startingIndex
		in: self container.
	wrapFlag ifFalse:
		["Was given huge container at first... now adjust"
		newParagraph adjustRightX].
	newParagraph focused: (self currentHand keyboardFocus == self).
	paragraph := newParagraph.
	self fit.
	self removeProperty: #CreatingParagraph.


	^ paragraph