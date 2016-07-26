initialization
initialize
	super initialize.
	shouldUpdateTitle := true.
	contentSelection := nil.
	acceptor := ClassDefinitionAcceptor model: self.
	fullDisplayedComment := false