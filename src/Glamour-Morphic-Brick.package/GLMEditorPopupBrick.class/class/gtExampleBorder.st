examples
gtExampleBorder
	<gtExample>
	<label: 'Example of editable popup with border'>

	^ GLMBrick new
		hShrinkWrap;
		vShrinkWrap;
		addBrickBack: (self createLabel: [ :b |
			self new
				in: [ :aPopup  | aPopup themer: aPopup themer popupEditorBorderThemer themer ];
				closer: (GLMPopupBrickCloser predicate: GLMPopupBrickOutisideClickPredicate new);
				textModel: 'haba';
				openOn: b ]);
		yourself