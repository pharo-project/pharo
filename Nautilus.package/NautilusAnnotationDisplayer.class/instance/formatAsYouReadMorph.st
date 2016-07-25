submorphs-accessing
formatAsYouReadMorph
	^ (CheckboxMorph on: self selected: #formatAsYouRead changeSelected: #toggleFormatAsYouRead)
		getLabelSelector: #formatAsYouReadLabel;
		labelClickable: true;
		vResizing: #shrinkWrap;
		hResizing: #shrinkWrap;
		setBalloonText:
			'Autoformat code without editing. Useful when reading some old/unformatted code. You can configure your own format in the settings browser.';
		yourself