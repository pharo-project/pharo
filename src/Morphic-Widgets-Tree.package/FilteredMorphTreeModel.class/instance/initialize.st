initialization
initialize
	super initialize.
	patternModel := RubScrolledTextModel new.
	patternModel announcer 
		when: RubTextUpdatedInModel 
		send: #whenPatternChanged: 
		to: self.
	autoAccept := true.
	ghostString := ''