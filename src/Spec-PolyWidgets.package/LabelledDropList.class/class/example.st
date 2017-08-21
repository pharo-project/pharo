example
example
	| example |
	example := self abstractExample.
example
		items: {'item 1' . 'item 2'};
		displayBlock: [ :item | item asString ];
		extent: 400 @ 50;
		openWithSpec.
	^ example