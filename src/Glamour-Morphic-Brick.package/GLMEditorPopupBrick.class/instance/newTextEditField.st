instance creation
newTextEditField

	^ GLMRubEditingAreaBrick new
		wrapped: false;
		hShrinkWrap;
		vShrinkWrap;
		withoutBorder;
		withoutMargins;
		beForSmalltalkCode;
		text: self textModel;
		yourself