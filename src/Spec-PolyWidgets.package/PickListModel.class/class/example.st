example
example
	| example |
	example := self new.
	example
		items:
			{{1 . 2 . 4}.
			'Foo'.
			123}.
	example openWithSpec.
			^example