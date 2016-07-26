label-brick-actions
beEdit

	self isEditMode ifTrue: [ ^ self ].
	self editBrick updateTextWith: self labelBrick text.
	self editBrick moveCursorToIndex: self labelBrick text size + 1.

	self addBrickFirst: self editBrick.
	self removeBrick: self labelBrick.
	self borderColor: (self themer focusedBorderColor alpha: 0).
	self editBrick rubric takeKeyboardFocus.
	self announce: #onEditMode