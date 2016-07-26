submorphs-accessing
noteNewOwner: aMorph
	super noteNewOwner: aMorph.
	ghostText := self paragraphProvider defaultGhostTextMorph.
	textColor := self defaultTextColor. 
	self textArea announcer when: MorphGotFocus send: #showHideGhostText to: self.	
	self textArea announcer when: MorphLostFocus send: #showHideGhostText to: self.