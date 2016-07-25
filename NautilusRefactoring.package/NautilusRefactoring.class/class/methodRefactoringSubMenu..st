menu
methodRefactoringSubMenu: aBuilder
	<nautilusGlobalMethodMenu>
	| target selectedMethod |

	target := aBuilder model.
	selectedMethod := target selectedMethod.
	selectedMethod ifNil:[ ^ target ].
	
	selectedMethod origin instanceSide methodRefactoring: aBuilder.