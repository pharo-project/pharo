text
helpText

	^ 'ShoreLine Reporter will send a report of the current exception to the ShoreLine server.
	
The report contains the wollowing information:
- the author name
- a timestamp of the exception
- the version of the current image
- the stack trace of the exception.

The stack trace is a list of method calls in the format ClassName>>methodName. No method parameter or personal data is collected.

You can see the full report in the report summary window.'