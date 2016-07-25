submorphs-accessing
lineAnnotationMorph
	^ (RubTextFieldArea new
		withoutAnyDecorator;
		backgroundColor: Color transparent;
		font: self fontToUse;
		beReadOnly;
		textColor: self textColor)
		setBalloonText: 'Current line number/Total line number [Current character number]';
		yourself