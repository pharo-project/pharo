build ui text
buildNewCommentText
	| scrolledText |
	scrolledText := self classCommentTextModel newScrolledText
		beForSmalltalkComment; 
		withCommentAnnotation;
		beWrapped;
		yourself.
	self classCommentTextModel setInitialText: self getComments.
	classCommentTextModel announcer
		when: RubTextAcceptRequest send: #whenTextUpdatedInCommentPane: to: self;
		when: NautilusExtendCommentSwitch send: #whenExtendCommentSwitch: to: self;
		when: RubTextUpdatedInModel send: #whenTextUpdatedInCommentPane: to: self.
	^ scrolledText