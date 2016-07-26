comparing
sortValue
	| v normalizedWeight | 
	normalizedWeight := weightValue.
	normalizedWeight = LogicalFont weightMedium 
		ifTrue:["sort medium and regular weights as though they were the same"
			normalizedWeight := LogicalFont weightRegular]. 
	v :=self simulated ifTrue:[10000] ifFalse:[0].
	v := v + (stretchValue * 1000).
	v := v + (normalizedWeight).
	v := v + (slantValue).
	^v
