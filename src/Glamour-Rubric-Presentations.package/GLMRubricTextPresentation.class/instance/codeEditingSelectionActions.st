accessing
codeEditingSelectionActions
	
	^ { 
		(GLMGenericAction new) 
				title: 'Cut'; 
				action: [ :aPresentation |
					aPresentation cut ]; 
				iconName: #smallCutIcon;
				shortcut: $x; 
				withSeparatorBefore;
				yourself.
		(GLMGenericAction new) 
				title: 'Copy';
				action: [ :aPresentation |
					aPresentation copySelection ]; 
				iconName: #smallCopyIcon;
				shortcut: $c; 
				yourself.
		(GLMGenericAction new) 
				title: 'Paste';
				action: [ :aPresentation |
					aPresentation paste ]; 
				iconName: #smallPasteIcon;
				shortcut: $v; 
				yourself.
		(GLMGenericAction new) 
				title: 'Paste...';
				action: [ :aPresentation |
					aPresentation pasteRecent  ]; 
				iconName: #smallCopyIcon;
				withSeparatorAfter;
				yourself. }