rendering
defaultSelectionActions

	^ { 
		(GLMGenericAction new) 
				title: 'Print it';
				action: [ :aPresentation |
					aPresentation printSelection ]; 
				shortcut: $p; 
				yourself.
		(GLMGenericAction new) 
				title: 'Do it';
				action: [ :aPresentation |
					aPresentation evaluateSelection ]; 
				shortcut: $d; 
				icon: GLMUIThemeExtraIcons glamorousSpawn;
				yourself.
		(GLMGenericAction new) 
				title: 'Inspect it'; 
				action: [ :aPresentation |
					aPresentation evaluateSelectionAndDo: [ :result | result inspect ] ]; 
				shortcut: $i; 
				icon: GLMUIThemeExtraIcons glamorousInspect;
				yourself.
		(GLMGenericAction new) 
				title: 'Debug it'; 
				action: [ :aPresentation |
					aPresentation debugSelection ]; 
				"shortcut: $D; "
				yourself.
		(GLMGenericAction new) 
				title: 'Profile it'; 
				action: [ :aPresentation |
					aPresentation profileSelection ]; 
				yourself
		 }