request handling
executeRequestResponseLoopOn: stream
	"Execute the HTTP request / response loop on stream one cycle at a time
	until the other end indicates it wants to stop or times out"

	ZnCurrentServer 
		value: self 
		during: [ 
			[ self executeOneRequestResponseOn: stream ] whileFalse ]