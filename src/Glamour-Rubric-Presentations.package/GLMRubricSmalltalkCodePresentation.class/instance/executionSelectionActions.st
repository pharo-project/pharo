accessing
executionSelectionActions

	^ { 
		(GLMGenericAction new) 
				title: 'Do it';
				action: [ :aPresentation |
					aPresentation highlightEvaluateAndDo: [ :result | ] ]; 
				keymap: PharoShortcuts current doItShortcut; 
				icon: GLMUIThemeExtraIcons glamorousSpawn;
				yourself.
		(GLMGenericAction new) 
				title: 'Print it';
				action: [ :aPresentation |
					aPresentation printSelection ]; 
				keymap: PharoShortcuts current printItShortcut; 
				yourself.
		(GLMGenericAction new) 
				title: 'Inspect it'; 
				action: [ :aPresentation |
					aPresentation highlightEvaluateAndDo: [ :result | result inspect ] ]; 
				keymap: PharoShortcuts current inspectItShortcut; 
				icon: GLMUIThemeExtraIcons glamorousInspect;
				yourself.
		(GLMGenericAction new) 
				title: 'Basic Inspect it'; 
				action: [ :aPresentation |
					aPresentation highlightEvaluateAndDo: [ :result | result basicInspect ] ]; 
				shortcut: $I; 
				icon: GLMUIThemeExtraIcons glamorousInspect;
				yourself.
		(GLMGenericAction new) 
				title: 'Debug it'; 
				action: [ :aPresentation |
					aPresentation debugSelection ]; 
				shortcut: $D;
				yourself.
		(GLMGenericAction new) 
				title: 'Profile it'; 
				action: [ :aPresentation |
					aPresentation profileSelection ]; 
				withSeparatorAfter;
				yourself.
		 }