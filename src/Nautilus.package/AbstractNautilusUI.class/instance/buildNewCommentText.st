build ui text
buildNewCommentText
	| scrolledText |
	scrolledText := self commentTextModel newScrolledText
		beForSmalltalkComment; 
		withCommentAnnotation;
		beWrapped;
		yourself.
	self commentTextModel setInitialText: self getComments.
	commentTextModel announcer
		when: RubTextAcceptRequest send: #whenTextUpdatedInCommentPane: to: self;
		when: NautilusExtendCommentSwitch send: #whenExtendCommentSwitch: to: self;
		when: RubTextUpdatedInModel send: #whenTextUpdatedInCommentPane: to: self.
	^ scrolledText