actions
closeActionFor: aPresentation

	^ GLMGenericAction new
			action: [ self removeLastPane ];
			icon: GLMUIThemeExtraIcons glamorousClose;
			condition: [ :currentPresentation |
				self panes size > 1 and: [ self panes last presentations = currentPresentation ] ];
			title: 'Close this inspector column'