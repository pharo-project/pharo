view
newScrolledText
	| scrolledText |
	scrolledText := self textModel
		newScrolledText
		beForSmalltalkCode;
		withCodeSizeFeedback;
		withGhostText: 'Choose a class and then a method to edit its code';
		yourself.
	scrolledText ghostTextRuler
		centered;
		fontPointSize: 18.
	^ scrolledText