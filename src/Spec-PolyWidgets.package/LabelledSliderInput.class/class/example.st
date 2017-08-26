example
example
	| example |
	example := self new
		min: 0;
		max: 250;
		autoAccept: true;
		value: 120;
		extent: 400 @ 50;
		title: 'LabelledSliderInput example';
		openWithSpec.
	^ example