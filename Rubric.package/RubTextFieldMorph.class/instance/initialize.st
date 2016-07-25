initialize-release
initialize
	super initialize.
	self textArea announcer when: RubReturnEntered  send: #whenReturnEntered: to: self.
	self scrollbarsShowNever.
	self extent: self extent