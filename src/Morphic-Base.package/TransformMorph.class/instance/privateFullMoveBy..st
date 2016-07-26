private
privateFullMoveBy: delta
	"Private! Relocate me, but not my subMorphs."

	self privateMoveBy: delta.
	transform :=  (transform asMorphicTransform) withOffset: (transform offset - delta).
