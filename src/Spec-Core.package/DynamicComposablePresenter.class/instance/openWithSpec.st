api
openWithSpec

	self layout ifNil: [ ^ super openWithSpec ].
	^ self openWithSpecLayout: self layout