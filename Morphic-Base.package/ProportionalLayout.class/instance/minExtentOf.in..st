layout
minExtentOf: aMorph in: newBounds
	"Return the minimal size aMorph's children would require given the new bounds"
	| min |
	min := 0@0.
	aMorph submorphsDo:[:m| | extent frame |
		"Map the minimal size of the child through the layout frame.
		Note: This is done here and not in the child because its specific
		for proportional layouts. Perhaps we'll generalize this for table
		layouts but I'm not sure how and when."
		extent := m minExtent.
		frame := m layoutFrame.
		frame ifNotNil:[extent := frame minExtentFrom: extent].
		min := min max: extent].
	^min