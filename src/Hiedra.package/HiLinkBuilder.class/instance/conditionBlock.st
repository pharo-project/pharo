accessing
conditionBlock

	^ conditionBlock ifNil: [ 
		conditionBlock := [ :node | true ] ]