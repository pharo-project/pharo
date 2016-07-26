accessing request
request: aRequest
	"Replace my current request with aRequest.
	This is a low level operation only needed in special cases."
	
	self validateConnectionForHost: aRequest url host port: aRequest url port.
	request := aRequest