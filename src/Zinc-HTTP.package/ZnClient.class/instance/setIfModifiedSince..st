accessing request
setIfModifiedSince: reference
	"Tell the server that we are only interested in the currently requested URI
	if it was modified since reference, a DateAndTime.
	If the URI was not modified since reference, the response will be #isNotModified."

	self request setIfModifiedSince: reference