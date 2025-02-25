"
ZnTooManyConcurrentConnections is signalled when the maximum number of concurrent connections of an HTTP Server is reached.

When this threshold is reached, a 503 Service Unavailable response will be sent after which the connection will be closed.

Part of Zinc HTTP Components.
"
Class {
	#name : 'ZnTooManyConcurrentConnections',
	#superclass : 'Error',
	#category : 'Zinc-HTTP-Exceptions',
	#package : 'Zinc-HTTP',
	#tag : 'Exceptions'
}
