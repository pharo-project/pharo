history
selectPreviousContent

	| trimmedContents  |
	trimmedContents := self lastContents collect: [:c | c copyFrom: 1 to: (c size min: 20)].
	^ (UIManager default chooseFrom: trimmedContents values: self lastContents)
		ifNotNil: [ :newContents | self setContent: newContents ]