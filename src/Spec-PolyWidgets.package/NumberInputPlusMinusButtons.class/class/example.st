examples
example
	| example |
	example := self new.
	example
		title: 'RGBWidget exampe';
		extent: 150 @ 100;
		value:6;
		valueToIncrement: 2;
		openWithSpec.
	^ example