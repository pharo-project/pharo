accessing request
method: httpMethod
	"Set the HTTP method of the current request to httpMethod.
	An uppercase symbol such as #GET is expected.
	Note that for all methods in #autoResetEntityMethods #resetEntity is called automatically,
	since you don't want the entity of a previous request to be sent again"

	(self autoResetEntityMethods includes: httpMethod)
		ifTrue: [ self resetEntity ].
	request method: httpMethod