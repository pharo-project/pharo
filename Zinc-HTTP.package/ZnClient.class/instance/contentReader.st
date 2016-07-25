options
contentReader
	"Return the current content reader, an object conforming 
	to the #value: protocol to convert a response entity
	as used by #contents. Defaults to nil.
	Can be used to parse incoming data."
	
	^ self optionAt: #contentReader ifAbsent: [ nil ]