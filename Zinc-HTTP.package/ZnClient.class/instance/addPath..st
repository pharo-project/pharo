accessing request
addPath: object
	"Extend the path of the current request with object.
	Either a string or a collection of strings is allowed as argument."
	
	object isString
		ifTrue: [ self addPathSegment: object ] 
		ifFalse: [ object do: [ :each | self addPathSegment: each ] ]