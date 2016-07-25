acccessing
handlesOperation: operation with: argument on: url
	"Return true when my instances can handle operation with argument on url.
	This can optionally be overridden for a more specific test."
	
	^ self operation = operation and: [ self schemes includes: url scheme ]