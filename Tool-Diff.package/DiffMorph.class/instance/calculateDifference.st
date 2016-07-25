actions
calculateDifference
	"Calculate the difference of the src and dst.
	Use src/dest morphs text, because we may want to compare the pretty printed text"

	self difference: ((TextDiffBuilder
		from: self srcMorph getText to: self dstMorph getText)
			buildPatchSequence)