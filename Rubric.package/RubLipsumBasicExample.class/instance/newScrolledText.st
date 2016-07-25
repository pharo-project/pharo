user interface
newScrolledText
	| st |
	st := self textModel newScrolledText
		withAdornment;
		withLineNumbers;
		withColumns;
		withGhostText: 'Hello, the text is now empty. Don''t worry, I will disappear if you enter some text.';
		withAnnotation;
		yourself.
	st ghostTextRuler fontName: 'Bitmap DejaVu Sans' pointSize: 18; centered.
	^ st