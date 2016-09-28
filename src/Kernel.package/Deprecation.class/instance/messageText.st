accessing
messageText
	"Return an exception's message text."

	^ 'The method ', self deprecatedMethodName, ' called from ', self sendingMethodName ,' has been deprecated.
', explanationString