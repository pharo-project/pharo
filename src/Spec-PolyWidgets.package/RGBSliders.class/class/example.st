example
example
	| example |
	example := self new.
	example
		title: 'RGBSliders example';
		extent: 300 @ 200;
		openDialogWithSpec.
	^ example