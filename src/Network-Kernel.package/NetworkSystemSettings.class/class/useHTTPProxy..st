settings
useHTTPProxy: aBoolean
	"Set whether an HTTP proxy should be used. This is a global switch."

	UseHTTPProxy = aBoolean ifTrue: [ ^ self ].
	UseHTTPProxy := aBoolean.
	self useHTTPProxy
		ifTrue: [
			self httpProxyPort: self lastHTTPProxyPort.
			self httpProxyServer: self lastHTTPProxyServer ]
		ifFalse: [
			self httpProxyPort: nil.
			self httpProxyServer: nil ]