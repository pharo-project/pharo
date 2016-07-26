options
useGzipCompressionAndChunking: boolean
	"Set whether we should try to use gzip content encoding and chunked transfer encoding, the default being false."
	
	self optionAt: #useGzipCompressionAndChunking put: boolean