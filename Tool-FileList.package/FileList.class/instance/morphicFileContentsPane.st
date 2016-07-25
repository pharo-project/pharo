user interface
morphicFileContentsPane
	| scrolledText |
	scrolledText := self sourceTextModel newScrolledText
		beWrapped;
		yourself.
	scrolledText menuProvider: self selector: #fileContentsMenu:shifted: .
	scrolledText announcer when: RubTextAccepted send: #whenTextAccepted: to: self.
	self sourceTextModel setText: self contents.
	^ scrolledText