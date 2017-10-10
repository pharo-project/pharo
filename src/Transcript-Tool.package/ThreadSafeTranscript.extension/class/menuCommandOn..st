*Transcript-Tool
menuCommandOn: aBuilder
	<worldMenu>
	(aBuilder item: #'Transcript')
		action: [ Smalltalk tools transcript open ];
		order: 0.30;
		parent: #Tools;
		help: 'Transcript';
		keyText: 'o, t';
		icon: self taskbarIcon.
	aBuilder withSeparatorAfter
