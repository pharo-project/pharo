tests - comparing
testEqualBlock
	| pluggableDict assoc |

	pluggableDict := self emptyDict.
	self assert: pluggableDict equalBlock isNil.
	self assert: (pluggableDict equalBlock: [:a :b | a = b]) == pluggableDict.

	assoc := 'newKey' -> 'newValue'.
	pluggableDict add: assoc.
	self assert: (pluggableDict includesAssociation: assoc).

	pluggableDict equalBlock: [:a :b | a == b].
	self assert: (pluggableDict includesAssociation: assoc).
	pluggableDict
		removeKey: assoc key;
		add: assoc key copy -> assoc value.
	self deny: (pluggableDict includesAssociation: assoc).

	pluggableDict equalBlock: [:a :b | a = b].
	self assert: (pluggableDict includesAssociation: assoc).