instance creation
new
	^ self named: (UUID new asString36 first: 6) 