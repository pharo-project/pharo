search
searchedFrame
	"Answer the frame of the selected morph in the receiver or nil if none."
	
	^searchedElement
		ifNotNil: [:elt | elt bounds: elt selectionFrame in: self]