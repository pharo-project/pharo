items creation
buildSourceTextArea
	sourceTextArea := self sourceTextModel newScrolledText
		beForSmalltalkCode;
		beWrapped;
		yourself.
	sourceTextArea textArea announcer when: RubTextAcceptRequest send: #whenTextAcceptedInView: to: self.
	self updateSourceCode.
	^ sourceTextArea