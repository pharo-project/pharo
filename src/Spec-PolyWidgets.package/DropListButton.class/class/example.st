example
example
	| example |
	example := self new.
	example
		displayBlock: [ :item | item asString ];
		items: {'Swordian' . 'Gardian' . 'Wizard' . 'Sniper'};
		label: 'Add';
		extent: 300 @ 70;
		title: 'DropListButton example';
		openWithSpec.
	^ example