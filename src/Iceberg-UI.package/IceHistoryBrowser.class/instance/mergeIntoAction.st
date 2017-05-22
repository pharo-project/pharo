action creations
mergeIntoAction
	^ GLMGenericAction new
		action: [ :presentation |
			self 
				mergeIntoAnotherBranch: presentation selection
				andThen: [
					presentation selection: nil.
					presentation pane browser update 
				]];
		icon: GLMUIThemeExtraIcons glamorousLeftSide;
		shortcut: $m;
		title: 'Merge into another branch...'; 
		showTitle