protocol
newHSplitterAt: anIndex

	| above frame splitter index size |
	size := submorphs size.
	index := size - anIndex + 2.
	above := self submorphs at: index.
	frame := above layoutFrame.
	splitter := self newHSplitter.
	splitter  layoutFrame:  ((frame leftFraction @ frame bottomFraction corner: frame rightFraction @ frame bottomFraction) asLayoutFrame 
										bottomOffset: 4).			
	self privateAddMorph: splitter atIndex: (size - anIndex + 2)