"
A trait for test purposes
"
Trait {
	#name : #TDictionaryComparingTest,
	#category : #Collections-Tests-Unordered
}

{ #category : #'tests - comparing' }
TDictionaryComparingTest >> testEquality [
	| nonEmptyDict2 |
	nonEmptyDict2 := self nonEmpty class new.
	self nonEmpty keysAndValuesDo:  [ :key :value | nonEmptyDict2 at: key put: value  ].
	
	self assert: (self nonEmptyDict = nonEmptyDict2)
]
