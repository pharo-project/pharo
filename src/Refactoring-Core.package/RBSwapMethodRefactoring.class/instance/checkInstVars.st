preconditions
checkInstVars
	class instanceVariableNames do: [ :each |
		(target instanceVariableNames includes: each) ifFalse: [
			((class whichSelectorsReferToInstanceVariable: each) includes: selector) ifTrue: [
				self
					refactoringError: ('<1p> refers to <2s>, which is not defined in <3p>' expandMacrosWith: selector with: each with: target) ] ] ]