actions
detachSession
	self flag: 'why silent value?'.
	(self browser pane port: #entity) silentValue: nil