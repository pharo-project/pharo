rendering
actOnPaneAdded: ann 
	tabs 
		addLazyPage: (GLMMorphicPaneWithoutTitleRenderer new render: ann pane) 
		label: (self titleOrIconOf: ann pane presentations in: tabs)
		toolbar: nil
		collapsable: true.
	tabs selectedPageIndex: tabs pages size