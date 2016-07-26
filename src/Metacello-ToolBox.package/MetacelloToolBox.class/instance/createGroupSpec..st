spec creation
createGroupSpec: baseName
	^ (self project groupSpec)
		name: baseName;
		yourself