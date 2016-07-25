user interface
newScrolledText
	| st |
	st := textModel newScrolledText 
		beWrapped;
		autoAccept: true;
		withLineNumbers;
		withGhostText: 'Workspace: a place where you can code and test scripts';
		withAnnotation;
		yourself.
	st ghostTextRuler fontName: 'Bitmap DejaVu Sans' pointSize: 18; textColor: Color gray lighter; centered.
	^st