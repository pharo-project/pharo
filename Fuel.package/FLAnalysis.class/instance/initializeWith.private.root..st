initialize-release
initializeWith: aMapper private: privateMapper root: anObject

	self initialize.
	firstMapper := aMapper.
	privateFirstMapper := privateMapper.
	aMapper analysis: self.
	privateMapper analysis: self.
	root := anObject.
	objectStack := FLSimpleStack new.
	privateObjectStack := FLSimpleStack new.
	clusterization := FLClusterization new.
