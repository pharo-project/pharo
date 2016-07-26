importing
handleMissingBehavior
	"self error: ('Cannot install method in unexistent behavior {1}' format: {(behaviorName asString)})"
	
	(MissingClassError 
		className: behaviorName asString)
		signal: 'Cannot install method in non-existent behavior ', behaviorName asString