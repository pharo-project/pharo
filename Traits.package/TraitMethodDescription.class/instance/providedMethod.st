accessing
providedMethod
	^self providedLocatedMethod ifNotNil: [:locatedMethod | locatedMethod method]