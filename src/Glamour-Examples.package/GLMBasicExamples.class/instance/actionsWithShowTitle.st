actions examples
actionsWithShowTitle
	<glmBrowser: 'Actions showing title' input: '''This example shows how actions can be rendered as buttons with text.'''>
	"self new actionsWithShowTitle openOn: 'This example shows how actions can be rendered as buttons with text.'"
	| browser |
	browser := GLMWrapper new.
	browser show: [:a | 
		a text 
			title: 'List';
			addAction: (GLMGenericAction new
			action: [  ];
			icon: GLMUIThemeExtraIcons glamorousBrowse;
			title: 'Action 1';
			showTitle);
		addAction: (GLMGenericAction new
			action: [  ];
			icon: GLMUIThemeExtraIcons glamorousBrowse;
			title: 'Action2')  ].
	^ browser