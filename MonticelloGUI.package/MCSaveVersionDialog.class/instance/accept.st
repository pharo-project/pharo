morphic ui
accept
	| version aMessage |
	version := (self findTextMorph: #versionName) text asString.
	aMessage := (self findTextMorph: #logMessage) text asString.
	self addAsLastLogMessage: aMessage.
	self answer: {version. aMessage}