editing
style: aText
	"we can do background styling with (self styler styleInBackgroundProcess: aText), but it is not safe"

	"on error just do nothing. Raising an error during styling may lead to an inconsistent
	state for the RubParagraph and its composer"

	[ ^ self styler style: aText ]
		on: Error
		do: [ :ex | 
			ex logCr.
			^ aText ]