order
privateActionIcon
	"Return the icon for this action"
	^ IconicButton new
		target: self;
		actionSelector: #run:;
		arguments: { method };
		labelGraphic: (self iconForMethod: method);
		color: Color transparent;
		helpText: 'Run';
		extent: 12 @ 12;
		borderWidth: 0