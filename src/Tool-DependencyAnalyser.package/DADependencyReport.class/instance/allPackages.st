accessing
allPackages
	^ Set new
		addAll: dependencies keys;
		addAll: dependencies values flattened;
		yourself
		