private
setupTLSTo: url
	(ZnNetworkingUtils shouldProxyUrl: url)
		ifTrue: [ | originalRequest |
			"http://www.ietf.org/rfc/rfc2817.txt (section 5)"
			"https://en.wikipedia.org/wiki/HTTP_tunnel#HTTP_CONNECT_tunneling"
			originalRequest := request copy.
			self method: #CONNECT; writeRequest; readResponse.
			response isSuccess ifFalse: [ self error: 'Failed to CONNECT to proxy for TLS/SSL' ].
			connection := ZnNetworkingUtils secureSocketStreamOn: connection socket.
			request := originalRequest ].
	
	connection sslSession certificateName: self certificate.
	
	[ connection sslSession serverName: url host ] 
		on: PrimitiveFailed 
		do: [ :error | "log it?" ].
		
	connection connect