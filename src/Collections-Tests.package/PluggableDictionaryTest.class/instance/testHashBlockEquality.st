tests - comparing
testHashBlockEquality
	| pluggableDict pluggableDict2 hashBlock |

	pluggableDict := self nonEmptyDict.
	pluggableDict2 := self nonEmptyDict copy.

	hashBlock := [:a | a hash].
	pluggableDict2 hashBlock: hashBlock.
	self deny: pluggableDict = pluggableDict2.
	self deny: pluggableDict2 = pluggableDict.

	pluggableDict hashBlock: hashBlock.
	self assert: pluggableDict = pluggableDict2.
	self assert: pluggableDict2 = pluggableDict.