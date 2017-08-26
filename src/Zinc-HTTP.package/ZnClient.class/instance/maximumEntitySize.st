options
maximumEntitySize
	"Return the maximum entity size in bytes that I will read from a stream before signalling ZnEntityTooLarge"
	
	^ self optionAt: #maximumEntitySize ifAbsent: [ ZnMaximumEntitySize value ]