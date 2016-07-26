buttons-adding
addButton: aBrick

	self buttons add: aBrick.
	
	aBrick enable.
	aBrick index: self buttons size.
	aBrick announcer when: GLMPagerButtonClicked send: #onButtonClicked: to: self.
	aBrick announcer when: GLMPagerButtonSelected send: #onButtonSelected: to: self.
	aBrick announcer when: GLMPagerButtonDeselected send: #onButtonDeselected to: self.
	
	self theme pagerWidgetThemer defaultScrollButtonStyleFor: aBrick.
	
	aBrick height: self pagerModel circleButtonSize.
	aBrick width: self pagerModel circleButtonSize.
	
	self addBrickBack: aBrick.
	
	(self buttons size = 1) ifTrue: [ self updateButtonsFocus ].
		
	self updateButtons.