converting
encode: string
	"Encode string using percent encoding and return the resulting string.
	I will use my character encoder to convert string to bytes and then
	percent encode all byte values that are not in my safe set."

	^ String streamContents: [ :stream | 
		self encode: string readStream to: stream ]