dropping/grabbing
mouseDown: evt
	super mouseDown: evt.
	self currentWindow ifNotNil: [ :topWindow | SystemWindow passivateTopWindow ]