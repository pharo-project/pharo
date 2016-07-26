instance creation
createCollapsable

	^ self new
		with: [ :collapsable | 
			collapsable headerBrick:
				(GLMBrick new
					addBrickBack:
						(GLMActionButtonBrick new
							icon: GLMUIThemeExtraIcons glamorousRight;
							marginLeft: 10;
							height: [ :b | b icon height * 1.2 ];
							width: [ :b | b icon width * 1.2 ];
							checkedImage: GLMUIThemeExtraIcons glamorousDown;
							action: [ :b | 
								collapsable toggle.
								collapsable isExpanded
									ifTrue: [ b check ]
									ifFalse: [ b uncheck ] ]);
					addBrickBack:
						(GLMLabelBrick new
							marginLeft: 10;
							text: 'Header';
							hSpaceFill;
							height: [ :b | b approximateHeight ]);
					vShrinkWrap;
					hSpaceFill) ];
		contentBrick:
			(GLMBrick new
				useVerticalLinearLayout;
				hSpaceFill;
				addAllBricks: {
					(GLMBrick new color: Color random; hSpaceFill).
					(GLMBrick new color: Color random; hSpaceFill).
					(GLMBrick new color: Color random; hSpaceFill).
					(GLMBrick new color: Color random; hSpaceFill)};
				yourself)