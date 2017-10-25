tests - new
testNewFrom
	| assocs |

	self assert: (self classToBeTested newFrom: {}) = self emptyDict.

	self assert: (self classToBeTested newFrom: nonEmptyDict copy) = self nonEmptyDict.

	assocs := self nonEmptyDict associations collect: [:each | each copy].
	self assert: (self classToBeTested newFrom: assocs) = self nonEmptyDict.
