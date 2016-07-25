accessing
state
	^ state ifNil: [self state: FindReplaceService new]