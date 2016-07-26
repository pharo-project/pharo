protocol
newVSplitterAt: anIndex
	| left frame splitter index size |

	size := submorphs size.
	index := size - anIndex + 2.
	left := self submorphs at: index.
	frame := left layoutFrame.
	
	splitter := self newVSplitter.
	splitter 
		layoutFrame: ((frame rightFraction @ frame topFraction corner: frame rightFraction @ frame bottomFraction) asLayoutFrame
							 rightOffset: 4).

	self privateAddMorph: splitter atIndex: (size - anIndex + 2)