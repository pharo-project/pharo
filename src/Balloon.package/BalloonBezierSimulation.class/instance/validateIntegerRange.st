private
validateIntegerRange
	self assert: [fwDx class == SmallInteger].
	self assert: [fwDy class == SmallInteger].
	self assert: [fwDDx class == SmallInteger].
	self assert: [fwDDy class == SmallInteger].
