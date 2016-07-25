accessing request
applicationFormUrlEncodedEntity
	"Initialize the current request to have an application/x-www-form-urlencoded entity.
	Return the existing entity if there is one. 
	Fail if another type was already set."
	
	request hasEntity
		ifTrue: [
			request entity contentType = ZnMimeType applicationFormUrlEncoded 
				ifFalse: [ self error: 'An incompatible entity was already set for the request' ] ]
		ifFalse: [
			request entity: ZnApplicationFormUrlEncodedEntity new ].
	^ request entity