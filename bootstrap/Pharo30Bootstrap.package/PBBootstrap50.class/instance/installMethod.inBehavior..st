running
installMethod: method inBehavior: theClass

	| timePerMethod |
	timePerMethod := [self objectSpace interpreter
		evaluateCode: 'theBehavior addSelectorSilently: selector withMethod: compiledMethod'
		withTemps: { 
			'theBehavior' -> theClass.
			'selector' -> (method selector asLiteralInObjectSpace: objectSpace).
			'compiledMethod' -> method }] timeToRun.
	
	statistics addMethodTime: timePerMethod.