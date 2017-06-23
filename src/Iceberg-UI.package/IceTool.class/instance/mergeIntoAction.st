action creators
mergeIntoAction
	^ GLMGenericAction new
		action: [ :presentation |
			IceMergeTool new 
				source: presentation selection;
				onMerge: [
					presentation selection: nil.
					presentation pane browser update 
				];
				run
		];
		icon: GLMUIThemeExtraIcons glamorousLeftSide;
		shortcut: $m;
		title: 'Merge into another branch...'; 
		showTitle