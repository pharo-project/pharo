instance creation
newToggleButtonFor: aCollapsable

	^ (GLMActionButtonBrick new
			icon: GLMUIThemeExtraIcons glamorousRight;
			checkedImage: GLMUIThemeExtraIcons glamorousDown;
			height: [ :b | b icon height * 1.2 ];
			width: [ :b | b icon width * 1.2 ];
			action: [ :b | 
				aCollapsable toggle.
				aCollapsable isExpanded
					ifTrue: [ b check ]
					ifFalse: [ b uncheck ] ])