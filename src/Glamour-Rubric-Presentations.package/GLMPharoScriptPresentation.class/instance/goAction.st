actions
goAction
	^ GLMGenericAction new
				title: 'Do it and go';
				action: [ :t :entity |
					t highlightEvaluateAndDo: [ :result | t selection: result ] ]; 
				shortcut: $g; 
				yourself.