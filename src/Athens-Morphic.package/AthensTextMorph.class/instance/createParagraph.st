as yet unclassified
createParagraph
	| newParagraph |
	self setDefaultContentsIfNil.

	"...Code here to recreate the paragraph..."
	newParagraph  := AthensParagraph new.
	
	newParagraph
		compose: text
		style: textStyle copy
		in: self container.
		
		
	wrapFlag ifFalse:
		["Was given huge container at first... now adjust"
		newParagraph adjustRightX].
	
	newParagraph focused: (self currentHand keyboardFocus == self).
	
	paragraph := newParagraph.
	self fit.

	^ paragraph