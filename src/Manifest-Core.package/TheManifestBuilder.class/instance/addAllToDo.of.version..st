adding/removing
addAllToDo: aSet of: ruleId version: versionId

	| selector |
	selector := self selectorToDoOf: ruleId version: versionId. 
	self addAllItem: aSet selector: selector. 