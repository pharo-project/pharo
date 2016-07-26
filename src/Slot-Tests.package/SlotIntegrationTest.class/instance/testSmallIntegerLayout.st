tests
testSmallIntegerLayout
	self assert: (SmallInteger classLayout isKindOf: ImmediateLayout).