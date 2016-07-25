private
createTextView
	| v |
	v := textModel newScrolledText
		withRuler: RubWorkspaceBar new;
		beWrapped;
		yourself.
	v announcer when: RubTextAccepted send: #whenTextAccepted: to: self.
	^ v