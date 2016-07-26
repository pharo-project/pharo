options
useGzipCompressionAndChunking
	"Return whether we should try to use gzip content encoding and chunked transfer encoding, the default is false."
	
	^ self optionAt: #useGzipCompressionAndChunking ifAbsent: [ false ] 