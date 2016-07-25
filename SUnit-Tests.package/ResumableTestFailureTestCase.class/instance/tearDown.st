running
tearDown
	self deny: 'You should not see me' = duplicateFailureLog last
			description: 'We saw the ''You should not see me'' failure'.
	self deny: 'You should see more than me' = duplicateFailureLog last
			description: 'We did not see more than a ''You should see more than me'' failure'.