private protocol
checkForPendingError
	"If data is waiting, check it to catch any error reports.
	In case the response is not an error, push it back."

	self stream isDataAvailable
		ifFalse: [^self].
	self fetchNextResponse.
	self
		checkResponse: self lastResponse
		onError: [:response | (TelnetProtocolError protocolInstance: self) signal]
		onWarning: [:response | (TelnetProtocolError protocolInstance: self) signal].
	"if we get here, it wasn't an error"
	self pushResponse: self lastResponse