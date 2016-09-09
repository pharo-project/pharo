class tests
testRemoveClassChange
	| st |
	st := RBClassModelFactory rbNamespace new.
	st removeClassNamed: self class name.
	self deny: (st includesClassNamed: self class name).
	self assert: (st classNamed: self class name) isNil