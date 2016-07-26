examples
gtExample
	<gtExample>
	<label: 'Action button with icon and dynamic background color'>
	
	^ self new
			action: [ :b | b gtDisplayString logCr ];
			hoverAction: [ :b | b gtDisplayString logCr ];
			icon: GLMUIThemeExtraIcons glamorousGrayCircleEmpty;
			hoverIcon: GLMUIThemeExtraIcons glamorousRedCircle;
			selectedColor: Color veryVeryLightGray whiter slightlyLighter;
			pressedColor: Color veryVeryLightGray;
			borderColor: Color veryVeryLightGray;
			borderWidth: 1;
			padding: 5