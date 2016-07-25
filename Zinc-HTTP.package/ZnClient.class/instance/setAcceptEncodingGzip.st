accessing request
setAcceptEncodingGzip
	"Give the server the hint that we accept gzip content encoding.
	When the server complies, important bandwidth reductions are possible."

	self request setAcceptEncodingGzip