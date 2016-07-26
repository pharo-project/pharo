accessing
codeEditingSelectionActions
	| uitheme |
	uitheme := Smalltalk ui icons.
	^ { 
		(GLMGenericAction new) 
				title: 'Cut';
				action: [ :aPresentation |
					aPresentation cut ]; 
				icon: (uitheme smallCutIcon);
				shortcut: $x; 
				withSeparatorBefore;
				yourself.
		(GLMGenericAction new) 
				title: 'Copy';
				action: [ :aPresentation |
					aPresentation copySelection ]; 
				icon: uitheme smallCopyIcon;
				shortcut: $c; 
				yourself.
		(GLMGenericAction new) 
				title: 'Paste';
				action: [ :aPresentation |
					aPresentation paste ]; 
				icon: uitheme smallPasteIcon;
				shortcut: $v; 
				yourself.
		(GLMGenericAction new) 
				title: 'Paste...';
				action: [ :aPresentation |
					aPresentation pasteRecent  ]; 
				icon: uitheme smallCopyIcon;
				withSeparatorAfter;
				yourself. }