basic
testInstantiation

	| messageTally |
	messageTally := MessageTally new.
	self assert: (messageTally time = 0).
	self assert: (messageTally tally = 0)