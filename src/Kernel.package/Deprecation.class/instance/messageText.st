accessing
messageText
	"Return an exception's message text."

	^ 'The method ', methodReference fullName, ' called from ', self sendingMethodName ,' has been deprecated.
', explanationString