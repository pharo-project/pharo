example
example
	| example |
	example := self abstractExample.
	example button label: 'I am a button'.
	example
		items: {'item 1' . 'item 2'};
		displayBlock: [ :item | item asString ];
		extent: 550 @ 50;
		openWithSpec.
	^ example