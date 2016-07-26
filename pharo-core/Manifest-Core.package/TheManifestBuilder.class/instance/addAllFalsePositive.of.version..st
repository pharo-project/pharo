adding/removing
addAllFalsePositive: fp of: ruleId version: versionId

	| selector  |	
	selector := self selectorFalsePositiveOf: ruleId version: versionId. 
	self addAllItem: fp selector: selector.
	fp do: [:each |
		 (self containsToDo: each onRule: ruleId version: versionId)
		ifTrue: [ self removeToDo: each of:  ruleId version: versionId ]]
	