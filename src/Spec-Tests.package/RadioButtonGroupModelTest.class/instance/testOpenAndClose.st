tests
testOpenAndClose
	self shouldnt: [ radioGroupModel openWithSpec ] raise: Error.
	window := radioGroupModel window