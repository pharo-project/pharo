testing
canOptimizeLoading
	"Answer wether I can provide a patch for the working copy without the usual diff pass"
	^ package hasWorkingCopy
		and: [package workingCopy modified not
			and: [package workingCopy ancestors includes: self baseInfo]]