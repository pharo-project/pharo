accessing
selectionFrame
	"Answer the frame of the selected morph in the receiver or nil if none."
	
	^selectedMorph
		ifNotNil: [selectedMorph bounds:  selectedMorph selectionFrame in: self]