event handling
whenSelectorAccepted
	| compiledMethod |
	self selectorAndClassAreCorrect
		ifTrue:
			[ 
			compiledMethod := self compiledMethodFromSearchFields.
			sourceCodePane behavior: compiledMethod methodClass.
			self formattedCheckBox state
				ifTrue: [ self formatSourceCode ]
				ifFalse: [ sourceCodePane text: compiledMethod sourceCode ] ]
		ifFalse: [ 
			sourceCodePane behavior: nil.
			sourceCodePane text: ' ' ]