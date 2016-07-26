private-method
privateInlineParameterFor: aMethod
	^ RBInlineParameterRefactoring
		model: environment
		inlineParameter:
			(self
				chooseFrom: aMethod argumentNames
				title: 'Choose a parameter:'
				lines: aMethod argumentNames)
		in: aMethod methodClass
		selector: aMethod selector