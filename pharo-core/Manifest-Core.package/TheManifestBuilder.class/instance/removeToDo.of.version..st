adding/removing
removeToDo: fp of: ruleId version: versionId
	
	| selector |
	selector := self selectorToDoOf:ruleId version: versionId. 
	self removeItem: fp selector: selector.
	