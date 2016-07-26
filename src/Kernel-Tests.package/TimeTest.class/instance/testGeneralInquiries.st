tests
testGeneralInquiries
	| date aDateAndTime |

	self should: [ self timeClass timeWords ] raise: MessageNotUnderstood.

	date := '2 June 1973' asDate.
	time := '4:02:47 am' asTime.
	aDateAndTime := self timeClass dateAndTimeFromSeconds: (2285280000 + 14567).
	self
		assert: aDateAndTime equals: {date. time}.
