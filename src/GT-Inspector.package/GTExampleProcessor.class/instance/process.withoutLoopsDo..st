private
process: anExample withoutLoopsDo: aBlock
	| value |
	" check whether we already scheduled the given example for processing -> prevent recursion "
	value := nil.
	processing do: [ :processingExample |
		(processingExample equalsExample: anExample) ifTrue: [
			^ self onRecursionError value: self value: anExample ] ].	
	processing add: anExample. " prepare processing this example "
	value := aBlock value. " process this example "
	processing remove: anExample. " done processing "
	^ value