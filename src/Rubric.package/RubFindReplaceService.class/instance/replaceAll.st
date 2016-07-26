services
replaceAll
	| startIdx |
	startIdx := self wrapAround ifTrue: [1] ifFalse: [self textArea editor selectionInterval first].
	self textArea replaceAll: self findRegex with: self replaceText startingAt: startIdx
