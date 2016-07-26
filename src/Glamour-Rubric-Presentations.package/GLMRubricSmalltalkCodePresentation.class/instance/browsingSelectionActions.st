accessing
browsingSelectionActions

	^ { (GLMGenericAction new) 
				title: 'Browse it';
				action: [ :aPresentation |
					aPresentation browseIt  ]; 
				shortcut: $b; 
				category: 'Code search...';
				yourself.
		(GLMGenericAction new) 
				title: 'Senders of it';
				action: [ :aPresentation |
					aPresentation sendersOfIt ]; 
				shortcut: $n; 
				category: 'Code search...';
				yourself.
		(GLMGenericAction new) 
				title: 'Implementors of it';
				action: [ :aPresentation |
					aPresentation implementorsOfIt ]; 
				shortcut: $m; 
				category: 'Code search...';
				yourself.
		(GLMGenericAction new) 
				title: 'References to it';
				action: [ :aPresentation |
					aPresentation referencesToIt ]; 
				"shortcut: $N; "
				category: 'Code search...';
				withSeparatorAfter;
				yourself.
		(GLMGenericAction new) 
				title: 'Extended string search'; 
				action: [ :aPresentation |
					aPresentation selectLine.
					self systemNavigation 
						browseMethodsWithString: aPresentation selectedText
						matchCase: false ]; 
				shortcut: $E;
				category: 'Code search...';
				yourself.
		(GLMGenericAction new) 
				title: 'Method source with it';
				action: [ :aPresentation |
					aPresentation selectLine.
					self systemNavigation 
						browseMethodsWithSourceString: aPresentation selectedText
						matchCase: false.]; 
				category: 'Code search...';
				withSeparatorAfter;
				yourself. }