accessing
textEditBrick: aBrick
	textEditBrick := aBrick.
	
	self contentBrick addBrickBack: self textEditBrick.
	self textEditBrick rubric announcer when: RubTextAcceptRequest send: #acceptContents: to: self