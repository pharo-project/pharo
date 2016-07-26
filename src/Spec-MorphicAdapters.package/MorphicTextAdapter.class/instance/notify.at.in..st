spec protocol
notify: errorMessage at: position in: sourceCode

	self widgetDo: [ :w | w notify: errorMessage at: position in: sourceCode ]