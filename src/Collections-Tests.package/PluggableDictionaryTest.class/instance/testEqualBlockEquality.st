tests - comparing
testEqualBlockEquality
	| pluggableDict pluggableDict2 equalBlock |

	pluggableDict := self nonEmptyDict.
	pluggableDict2 := self nonEmptyDict copy.

	equalBlock := [:a :b | a = b].
	pluggableDict2 equalBlock: equalBlock.
	self deny: pluggableDict = pluggableDict2.
	self deny: pluggableDict2 = pluggableDict.

	pluggableDict equalBlock: equalBlock.
	self assert: pluggableDict = pluggableDict2.
	self assert: pluggableDict2 = pluggableDict.