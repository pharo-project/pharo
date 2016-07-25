adding/removing
removeAllToDo: fp of: ruleId version: versionId
	
	| selector |
	selector := self selectorToDoOf: ruleId version: versionId. 
	self removeAllItem: fp selector: selector.
	