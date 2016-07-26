option
refactoringOptions: aRefactoring
	aRefactoring
		setOption: #implementorToInline
		toUse: [ :ref :imps | self requestImplementorToInline: imps ];
		setOption: #methodName
		toUse: [ :ref :name | self requestMethodNameFor: name ];
		setOption: #selfArgumentName
		toUse: [ :ref | self requestSelfArgumentName ];
		setOption: #selectVariableToMoveTo
		toUse: [ :ref :class :selector | self selectVariableToMoveMethodTo: selector class: class ];
		setOption: #variableTypes
		toUse: [ :ref :types :selected | self selectVariableTypesFrom: types selected: selected for: ref ];
		setOption: #extractAssignment
		toUse: [ :ref :string | self shouldExtractAssignmentTo: string ];
		setOption: #inlineExpression
		toUse: [ :ref :string | self shouldInlineExpression: string ];
		setOption: #alreadyDefined
		toUse: [ :ref :class :selector | self shouldOverride: selector in: class ];
		setOption: #useExistingMethod
		toUse: [ :ref :selector | self shouldUseExistingMethod: selector ];
		setOption: #openBrowser
		toUse: [ :ref :env | self openEnvironment: env  for: ref]