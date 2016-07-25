accessing composer
uptodateComposer
	^ self composer
		text: self text;
		textStyle: self textStyle;
		container: self compositionRectangle;
		emphasisHere: textArea emphasisHere;
		cursorWidth: textArea cursorWidth;
		yourself