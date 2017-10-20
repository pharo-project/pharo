example
example
	| example |
	example := self new.
	example
		title: 'RGBWidget exampe';
		extent: 300 @ 250;
		openDialogWithSpec.
	^ example