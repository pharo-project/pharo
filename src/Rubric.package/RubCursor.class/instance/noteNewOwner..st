submorphs-accessing
noteNewOwner: aMorph 
	super noteNewOwner: aMorph.
	self textArea announcer when: MorphGotFocus send: #whenGotFocus: to: self.
	self textArea announcer when: MorphLostFocus send: #whenLostFocus: to: self.
	self textArea announcer when: RubExtentChanged send: #whenTextAreaExtentChanged: to: self.
	self textArea announcer when: RubSelectionChanged send: #whenTextAreaSelectionChanged: to: self.
	self textArea announcer when: RubTextStyleChanged send: #whenTextAreaTextStyleChanged: to: self.
