instance creation
newScrolledText
	| scrolledText |
	scrolledText := self textModel
		newScrolledText
		beForSmalltalkCode;
		beWrapped;
		beReadOnly;
		yourself.
	^ scrolledText