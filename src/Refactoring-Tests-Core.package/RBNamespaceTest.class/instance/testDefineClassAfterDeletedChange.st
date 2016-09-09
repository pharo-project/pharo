class tests
testDefineClassAfterDeletedChange
	| st |
	st := RBClassModelFactory rbNamespace new.
	st removeClassNamed: self class name.
	self deny: (st includesClassNamed: self class name).
	st defineClass: self class definition.
	self assert: (st includesClassNamed: self class name).
	self assert: (st classNamed: self class name) notNil