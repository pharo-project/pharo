morphic ui
enterIssueNumber
	| issue |
	issue := UIManager default request: 'Please give the issue number' initialAnswer: '00000'.
	issue isEmptyOrNil ifFalse: [
		issue isAllDigits ifFalse: [
			UIManager default inform: 'You must give us a number'.
			issue := self enterIssueNumber]].
	^ issue
	