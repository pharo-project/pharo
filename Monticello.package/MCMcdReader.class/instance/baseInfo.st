accessing
baseInfo
	^ baseInfo ifNil: [self loadBaseInfo]