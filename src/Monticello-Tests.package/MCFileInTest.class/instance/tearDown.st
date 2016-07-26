running
tearDown
	(diff isNil or: [diff isEmpty not])
		 ifTrue: [expected updatePackage: self mockPackage].
	MCDataStream initialize