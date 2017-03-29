actions
popupActOn: aPresentation from: aButton

	GLMPopupActionBrick new
		closer: (GLMPopupBrickCloser predicate: GLMPopupBrickOutisideClickPredicate new);
		onCreated: [ :popup |self action glamourValueWithArgs: (Array 
				with: aPresentation 
				withAll: { aPresentation entity . popup }) ];
		openOn: aButton;
		beRightToLeftWithOffset: 0