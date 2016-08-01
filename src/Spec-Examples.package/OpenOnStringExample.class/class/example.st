examples
example
	| example |
	example := self new: 'Hello world'.
	example
		extent: 300 @ 200;
		openWithSpec.
	^ example