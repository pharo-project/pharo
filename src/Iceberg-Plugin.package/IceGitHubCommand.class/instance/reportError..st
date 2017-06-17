private
reportError: error
	UIManager default 
		alert: error messageBody
		title: 'Error creating pull request: ', error messageText