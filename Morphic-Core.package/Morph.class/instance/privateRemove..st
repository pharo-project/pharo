private
privateRemove: aMorph
	"Private! Should only be used by methods that maintain the ower/submorph invariant."

	submorphs := submorphs copyWithout: aMorph.
	self layoutChanged.