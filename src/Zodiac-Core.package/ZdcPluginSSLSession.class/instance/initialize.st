initialize
initialize
	"Initialize the receiver"

	[ handle := self primitiveSSLCreate ]
		on: PrimitiveFailed 
		do: [ :exception |
			"Give a more human friendly error message"
			ZdcPluginMissing signal ].
	self logging: false
