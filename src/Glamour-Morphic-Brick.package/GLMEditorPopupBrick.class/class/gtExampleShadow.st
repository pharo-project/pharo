examples
gtExampleShadow
	<gtExample>
	<label: 'Example of editable popup with shadow'>

	^ GLMBrick new
		hShrinkWrap;
		vShrinkWrap;
		addBrickBack: (self createLabel: [ :b |
			self new
				in: [ :aPopup  | aPopup themer: aPopup themer popupEditorShadowThemer themer ];
				closer: (GLMPopupBrickCloser predicate: GLMPopupBrickOutisideClickPredicate new);
				textModel: 'haba';
				openOn: b ]);
		yourself