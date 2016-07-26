accessing request
multiPartFormDataEntity
	"Initialize the current request to have an multipart/form-data entity.
	Return the existing entity if there is one. 
	Fail if another type was already set."

	request hasEntity
		ifTrue: [
			(request entity contentType matches: ZnMimeType multiPartFormData)
				ifFalse: [ self error: 'An incompatible entity was already set for the request' ] ]
		ifFalse: [
			request entity: ZnMultiPartFormDataEntity new ].
	^ request entity