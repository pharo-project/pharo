accessing
methodGroupManager
	^ methodGroup ifNil: [ methodGroup := self buildMethodGroupManager ]