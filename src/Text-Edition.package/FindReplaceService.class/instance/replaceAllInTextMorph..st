services
replaceAllInTextMorph: aTextMorph
	| startIdx |
	startIdx := self wrapAround ifTrue: [1] ifFalse: [aTextMorph editor selectionInterval first].
	aTextMorph replaceAll: self findRegex with: self replaceText startingAt: startIdx
