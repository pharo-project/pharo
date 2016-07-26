packing
unpack: aByteArray
	| exceptions |
	exceptions := OrderedCollection new: self packings size.
	self packings do: [ :eachEventPacking |
		[ ^ eachEventPacking unpack: aByteArray ] on: Error
			do: [ :exception | exceptions addLast: exception ] ].
	exceptions
		ifEmpty: [ GTEventInitializationError signal: 'Uninitialized unpacking system' ]
		ifNotEmpty: [ GTEventUnpackingError 
								signal: 'Cannot unpack data' 
								data: aByteArray 
								errors: exceptions ].
	"In case of resumed exception it returns nil object."
	^ nil