initialization
initialize

	super initialize.

	selector := #newVSplitterAt:.
	layoutFrame := ((0@0 corner: 0@0) asSpecLayoutFrame 
							rightOffset: 1).
	commands := nil.