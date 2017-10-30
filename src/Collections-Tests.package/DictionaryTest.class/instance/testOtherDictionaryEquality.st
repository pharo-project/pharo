tests - comparing
testOtherDictionaryEquality
	self otherDictionaryClasses do: [:each | | nonEmptyDict2 |
		nonEmptyDict2 := each new.
		self deny: self empty = nonEmptyDict2.
		self deny: nonEmptyDict2 = self empty.

		self nonEmptyDict keysAndValuesDo: [:key :value | nonEmptyDict2 at: key put: value].
		self deny: self nonEmptyDict = nonEmptyDict2.
		self deny: nonEmptyDict2 = self nonEmptyDict]