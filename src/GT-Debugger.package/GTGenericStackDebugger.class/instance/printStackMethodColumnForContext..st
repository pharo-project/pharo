printing/formatting
printStackMethodColumnForContext: aContext 
	| outer selector |
	
	outer := aContext outerMostContext.
	^ String streamContents: [ :aStream |
		selector := outer selector ifNil: [ outer method defaultSelector ].
		aStream nextPutAll: selector ]