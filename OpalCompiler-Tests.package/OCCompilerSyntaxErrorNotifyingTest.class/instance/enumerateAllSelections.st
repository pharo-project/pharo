private
enumerateAllSelections
	"This method intercepts the SyntaxErrorNotification and prevent the SyntaxError morph to open.
	The notification errorCode hold the source of evaluated sub-selection with inserted error message.
	This can be compared to expected error notification."
	1 to: self numberOfSelections do: [:n |
		| result |
		result := [self evaluateSelectionNumber: n] on: SyntaxErrorNotification do: [:exc |
			| expectedNotification expectedNotificationLocation |
			expectedNotification := (expectedErrors at: n) allButFirst allButLast: 3.
			expectedNotificationLocation := (expectedErrorPositions at: n) - (morph editor startIndex - 1).
			self assert: expectedNotificationLocation equals: exc location.
			self assert: expectedNotification equals: exc errorMessage asString.
			exc return: nil]].