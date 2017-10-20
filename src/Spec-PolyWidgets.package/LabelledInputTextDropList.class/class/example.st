example
example
	| example |
	example := self abstractExample.
	example
		ghostText: 'ghost text';
		items: {'item 1' . 'item 2'};
		displayBlock: [ :item | item asString ];
		extent: 450 @ 50;
		openWithSpec.
	^ example