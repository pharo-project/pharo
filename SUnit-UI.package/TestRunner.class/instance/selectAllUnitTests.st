accessing-classes
selectAllUnitTests
	classesSelected := classes asSet select: [ :eachClass | eachClass isUnitTest ].
	self
		changed: #allSelections;
		changed: #classSelected;
		changed: #hasRunnable