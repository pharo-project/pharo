example
example
	| example |
	example := self abstractExample.
	example
		items: {'item 1' . 'item 2'};
		openWithSpec.
	^ example