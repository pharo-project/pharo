instance creation
newTriggerBrick

	^ GLMActionButtonBrick new
		icon: (self anchorBrick
			ifNil: [ GLMUIThemeExtraIcons glamorousExample ]
			ifNotNil: [ self anchorBrick brickTransparentImageForm ]);
		width: [ :b | b icon width ];
		height: [ :b | b icon height ]