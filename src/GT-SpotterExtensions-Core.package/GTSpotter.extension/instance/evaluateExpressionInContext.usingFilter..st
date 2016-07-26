*GT-SpotterExtensions-Core-private
evaluateExpressionInContext: aSpotterContext usingFilter: filter
	" only allow arithmethic selectors to be evaluated - otherwise this may have some severe side-effects "
	| result |
	" filter allowed selectors "
	(Number withAllSubclasses anySatisfy: [ :class |
		class selectors anySatisfy: [ :selector | 
			aSpotterContext textTrimmed includesSubstring: selector caseSensitive: true ] ]) ifFalse: [ ^ self ].
	" perform the selector "
	Smalltalk compiler 
		silentlyDo: [ result := Smalltalk compiler evaluate: aSpotterContext textTrimmed ] 
		exceptionDo: [ ^ self ].
	result isNumber ifFalse: [ ^ self ].
	filter value: result. 
	" if the result is not readable as float, append the float as well "
	(result isFraction and: [ result isInteger not ]) ifTrue: [
		filter value: result asFloat ]