submorphs-accessing
lineNumbersDisplayLabelMorph
	^ (StringMorph contents: self lineNumbersDisplayLabel font: self fontToUse)
		setBalloonText: 'Let you decide if the code pane should show the line numbers at the left of the code pane or not. +L: Click to add the lines number/L: Click to hide them.';
		yourself