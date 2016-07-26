accessing
session: aSession

	(self browser pane port: #entity) value: aSession.
	self title: aSession name