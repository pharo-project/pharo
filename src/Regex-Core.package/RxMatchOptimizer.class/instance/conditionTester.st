accessing
conditionTester
	"#any condition is filtered at the higher level;
	it cannot appear among the conditions here."

	| matchCondition |
	conditions isEmpty ifTrue: [^nil].
	conditions size = 1 ifTrue:
		[matchCondition := conditions detect: [:ignored | true].
		"Special case all of the possible conditions."
		#atBeginningOfLine = matchCondition ifTrue: [^[:c :matcher | matcher atBeginningOfLine]].
		#atEndOfLine = matchCondition ifTrue: [^[:c :matcher | matcher atEndOfLine]].
		#atBeginningOfWord = matchCondition ifTrue: [^[:c :matcher | matcher atBeginningOfWord]].
		#atEndOfWord = matchCondition ifTrue: [^[:c :matcher | matcher atEndOfWord]].
		#atWordBoundary = matchCondition ifTrue: [^[:c :matcher | matcher atWordBoundary]].
		#notAtWordBoundary = matchCondition ifTrue: [^[:c :matcher | matcher notAtWordBoundary]].
		RxParser signalCompilationException: 'invalid match condition'].
	"More than one condition. Capture them as an array in scope."
	matchCondition := conditions asArray.
	^[:c :matcher |
		matchCondition contains:
			[:conditionSelector |
			matcher perform: conditionSelector]]