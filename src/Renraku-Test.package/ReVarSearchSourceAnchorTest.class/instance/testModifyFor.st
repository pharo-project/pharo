tests
testModifyFor
	| anchor |
	anchor := ReVarSearchSourceAnchor new.
	
	self
		assert: (anchor modify: 'one' for: #(one))
		equals: '''one'''.
	
	self
		assert: (anchor modify: 'one' for: #(one two))
		equals: '''one '.
	
	self
		assert: (anchor modify: 'two' for: #(one two))
		equals: ' two'''.
	
	self
		assert: (anchor modify: 'two' for: #(one two three))
		equals: ' two '