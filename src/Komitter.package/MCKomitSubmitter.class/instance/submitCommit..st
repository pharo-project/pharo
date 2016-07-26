protocol
submitCommit: aKomit

	MCSaveVersionDialog addAsLastLogMessage: aKomit message asString.
	aKomit commitOn: self
	