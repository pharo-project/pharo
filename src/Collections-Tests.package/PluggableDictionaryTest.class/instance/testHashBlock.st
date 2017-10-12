tests - comparing
testHashBlock
	| pluggableDict keys |

	pluggableDict := self nonEmptyDict.
	keys := pluggableDict keys.
	self assert: pluggableDict hashBlock isNil.
	self assert: (pluggableDict hashBlock: [:a | a hash]) == pluggableDict.

	keys do: [:each | self assert: (pluggableDict includesKey: each)].
	self deny: (pluggableDict includesKey: self keyNotIn).