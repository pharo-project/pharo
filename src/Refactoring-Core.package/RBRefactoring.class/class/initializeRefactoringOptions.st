initialization
initializeRefactoringOptions
	RefactoringOptions := IdentityDictionary new.
	RefactoringOptions
		at: #implementorToInline
		put: [ :ref :imps | self error: #implementorToInline ];
		
		at: #methodName
		put: [ :ref :methodName | self error: #methodName ];
		
		at: #selfArgumentName
		put: [ :ref | self error: #selfArgumentName ];
		
		at: #selectVariableToMoveTo
		put: [ :ref :class :selector | self error: #selectVariableToMoveTo ];	
			
		at: #variableTypes
		put: [ :ref :types :selected | self error: #variableTypes ];
		
		at: #extractAssignment
		put: [ :ref :varName | self error: #extractAssignment ];
		
		at: #inlineExpression
		put: [ :ref :string | self error: #inlineExpression ];
		
		at: #alreadyDefined
		put: [ :ref :cls :selector |  self error: #alreadyDefined ];
		
		at: #useExistingMethod
		put: [ :ref :selector |  self error: #useExistingMethod ];
		
		at: #openBrowser
		put: [ :ref :env |  self error: #openBrowser ]