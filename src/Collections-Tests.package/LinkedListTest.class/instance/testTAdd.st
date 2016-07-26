tests - adding
testTAdd

	| added |
	added := self otherCollection add: self element.
	self assert: added = self element. "equality or identity ?"
	self assert: (self otherCollection includes: self element).

	