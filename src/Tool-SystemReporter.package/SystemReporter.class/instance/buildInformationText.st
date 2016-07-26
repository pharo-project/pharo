items creation
buildInformationText

	| scrolledText |
	scrolledText := self sourceTextModel newScrolledText
		beForPlainText;
		beReadOnly;
		beWrapped;
		yourself.
	^ scrolledText