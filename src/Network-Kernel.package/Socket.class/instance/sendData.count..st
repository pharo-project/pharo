sending
sendData: buffer count: n
	"Send the amount of data from the given buffer"
	| sent |
	sent := 0.
	[sent < n] whileTrue:[
		sent := sent + (self sendSomeData: buffer startIndex: sent+1 count: (n-sent))].