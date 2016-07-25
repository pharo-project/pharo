accessing request
addPathSegment: pathSegment
	"Extend the path of the current request with a single pathSegment."
	
	request url 
		ifNil: [ request url: (ZnUrl new addPathSegment: pathSegment; yourself) ]
		ifNotNil: [ request url addPathSegment: pathSegment ]  