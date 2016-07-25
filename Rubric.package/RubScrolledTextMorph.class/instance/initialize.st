initialize-release
initialize
	super initialize.
	self clipSubmorphs: true.
	enabled := true.
	rulers := SortedCollection sortBlock: [ :a :b | b level > a level ].
	self addMorph: (scrollPane := self newScrollPane).
	self announcer when: RubConfigurationChange send: #whenConfigurationChanged: to: self.
	self textArea announcer when: RubCancelEditRequested send: #whenCancelEditRequested: to: self.
	self textArea announcer when: MorphGotFocus send: #whenGotForcus: to: self.
	self textArea announcer when: MorphLostFocus send: #whenLostFocus: to: self.
	self textArea announcer when: RubTextAcceptRequest send: #whenTextAcceptRequest: to: self.
	self textArea announcer when: RubTextChanged send: #whenTextChangedInTextArea: to: self.
	self textArea announcer when: RubReturnEntered send: #whenReturnEnteredInTextArea: to: self.
	self textArea announcer when: RubKeystroke send: #whenKeystrokeInTextArea: to: self.
	self withAdornment.
	self borderStyle: self borderStyleToUse.
	highlights := OrderedCollection new