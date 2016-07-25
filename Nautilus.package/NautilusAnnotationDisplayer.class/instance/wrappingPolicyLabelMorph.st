submorphs-accessing
wrappingPolicyLabelMorph
	^ (StringMorph contents: self wrappingPolicyLabel font: self fontToUse)
		setBalloonText:
			'Let you decide if the code pane should automaticaly wrap lines if a line reach the end of the line. W: Wrap / NW: Add a scrollbar.';
		yourself