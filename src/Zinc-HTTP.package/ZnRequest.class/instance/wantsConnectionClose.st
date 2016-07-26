testing
wantsConnectionClose
	"Return if the HTTP protocol should close the connection after processing the receiver.
	Overwritten to return true in case of the presense of an explicit connection close request header
	or the absense of a keep alive header when using the old HTTP 1.0 protocol."

	^ self isConnectionClose 
		or: [ self isHttp10 and: [ self isConnectionKeepAlive not ] ]