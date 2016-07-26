build ui text
buildNewCodeSourceAreaRubric
	| scrolledText |
	scrolledText := self sourceTextModel newScrolledText
		withSourceCodeAnnotation;
		beForSmalltalkCode;
		beWrapped;
		yourself.
	self updateSourceCodeAreaWith: self sourceCode.
	scrolledText textArea announcer when: RubSelectionChanged send: #whenTextAreaSelectionChanged: to: self.
	self sourceTextModel announcer 
		when: RubTextAcceptRequest send: #whenTextAcceptedInView: to: self;
		when: NautilusClassSelected , NautilusPackageSelected , NautilusGroupSelected send: #showScriptCode: to: self;
		when: NautilusProtocolSelected , NautilusMethodSelected send: #showSourceCode: to: self.
	self setSourceCodeShorcutsTo: scrolledText.
	scrolledText menuProvider: self selector: #sourceCodeMenu:shifted:.
	^ scrolledText