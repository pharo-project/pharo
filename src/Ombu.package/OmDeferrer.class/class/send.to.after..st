instance creation
send: selector to: object after: aDuration

	^ self basicNew
		initializeWithSelector: selector
		object: object
		duration: aDuration