*NautilusRefactoring
searchCode: aText
	acceptor := CodeSearchingAcceptor model: self.
	self setStylerClass: nil.
	self pendingText: aText