action creators
mergeAction
	^ GLMGenericAction new
		action: [ :presentation |
			presentation selection merge.
			presentation selection: nil.
			presentation pane browser update. ];
		condition: [ :presentation | 
			presentation selection notNil 
				and: [ presentation selection isMerged not ] ];
		icon: GLMUIThemeExtraIcons glamorousLeftSide;
		shortcut: $m;
		title: 'Merge and load'; showTitle