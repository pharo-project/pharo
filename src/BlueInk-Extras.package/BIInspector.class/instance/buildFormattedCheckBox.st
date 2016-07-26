item creation
buildFormattedCheckBox
	^ self newCheckBox
		label: 'Formatted';
		state: true;
		whenActivatedDo:
				[ 
			formattedCheckBox toggleState.
			self selectorAndClassAreCorrect
				ifTrue: [ self formatSourceCode ] ];
		whenDeactivatedDo:
				[ 
			formattedCheckBox toggleState.
			self selectorAndClassAreCorrect 
				ifTrue: [ sourceCodePane text: self compiledMethodFromSearchFields sourceCode ] ];
		yourself