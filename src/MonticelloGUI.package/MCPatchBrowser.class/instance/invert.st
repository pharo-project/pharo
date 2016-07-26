selecting
invert
	items := items collect: [:ea | ea inverse].
	self changed: #list; changed: #text; changed: #selection