morphic geometry
privateFullMoveBy: delta
	"Private! Relocate me and all of my subMorphs by recursion. Subclasses that implement different coordinate systems may override this method."
	self privateMoveBy: delta.
