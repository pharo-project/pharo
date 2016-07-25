private
addActionsFor: aTab
	| rightOffset |
	
	rightOffset := self actionOffset.
	
	aTab hasMenu
		ifTrue: [ 
			rightOffset := 2 * rightOffset + self menuButtonWidth.
			self addMenuButton ].
		
	aTab actions reverseDo: [ :each || topOffset |
		topOffset := self height - each icon height // 2.
		self 
			addMorph: (self createActionButtonFor: each)
			fullFrame: (LayoutFrame identity
				bottomFraction: 0;
				leftFraction: 1;
				topOffset: topOffset;
				bottomOffset: topOffset + each icon height;
				rightOffset: rightOffset negated ;
				leftOffset: rightOffset negated - each icon width ).
				
		rightOffset := rightOffset + each icon width + self actionOffset]