examples
createButton: hoverAction

	^ (GLMActionButtonBrick new
			color: Color white;
			icon: GLMUIThemeExtraIcons glamorousBrowse;
			hoverAction: hoverAction;
			width: [ :b | b icon width * 2 ];
			height: [ :b | b icon height * 2 ])