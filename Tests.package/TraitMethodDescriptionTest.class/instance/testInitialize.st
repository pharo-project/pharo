testing
testInitialize
	| empty |
	empty := TraitMethodDescription new.
	self assert: empty isEmpty.
	self deny: empty isConflict.
	self deny: empty isProvided.
	self deny: empty isRequired